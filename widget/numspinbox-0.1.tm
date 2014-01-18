package require Tk
package require Ttk
package require snit

snit::widgetadaptor widget::numspinbox {

    constructor args {
        if { ! [dict exists $args -from]} {
            dict set args -from 0
        } else {
            if {![string is double -strict [dict get $args -from]]} {
                return -code error "expecting floating point number, but got \"[dict get $args -from]\""
            }
        }

        if { ! [dict exists $args -to]} {
            # 8 - bit per byte,
            # so set to maximum integer for this platform
            # todo: need to check for bigEndian platforms
            dict set args -to [expr {1<<($::tcl_platform(wordSize) * 8)}]
        } else {
            if {![string is double -strict [dict get $args -to]]} {
                return -code error "expecting floating point number, but got \"[dict get $args -to]\""
            }
        }

        if { ! [dict exists $args -format]} {
            # calculate most wide format using -from, -to and -increment
            # for this compare length of fractional parts
            set digits [expr {max([string length [lindex [split [dict get $args -from] .] 1]],
                                  [string length [lindex [split [dict get $args -to  ] .] 1]]
                                  )}]

            if {[dict exists $args -increment]} {
                set digits [expr {max($digits,[string length [lindex [split [dict get $args -increment] .] 1]])}]
            }

            dict set args -format %.${digits}f
        } else {
            regexp {^%(\d*)\.(\d*)f$} [dict get $args -format] -> -> digits
        }

        if { ! [dict exists $args -width]} {
            set values [list]
            foreach name {-from -to} {
                lappend values [string length [format [dict get $args -format] [dict get $args $name]]]
            }
            set width [::tcl::mathfunc::max {*}$values]
        } else {
            return -code error "unknown option -width"
        }

        if { ! [dict exists $args -increment]} {
            dict set args -increment [expr {10.0**(-$digits)}]
        }

        dict set args -increment [regsub {\.0*$} [dict get $args -increment] {}]

        installhull using spinbox {*}$args
        $hull configure -width $width

        # copy Spinbox binding to Numspinbox
        foreach b [bind Spinbox] {
            bind Numspinbox $b [bind Spinbox $b]
        }

        bindtags $win [list $win Numspinbox . all]

        bind Numspinbox <FocusOut> [list apply {{win args} {
            if {!([string is double -strict [$win get]] &&
                  [$win get] >= [$win cget -from] &&
                  [$win get] <= [$win cget -to])} {

                focus -force $win
                return -code break
            }
        }} %W]

        bind Numspinbox <Up>   [list + after idle $win CheckRange]
        bind Numspinbox <Down> [list + after idle $win CheckRange]

        bind Numspinbox <Control-End>  {
            %W set [format [%W cget -format] [%W cget -to]]
            event generate %W <<NumspinboxChanged>> -data [%W get]
        }

        bind Numspinbox <Control-Home> {
            %W set [format [%W cget -format] [%W cget -from]]
            event generate %W <<NumspinboxChanged>> -data [%W get]
        }

        bind Numspinbox <Return> {
            if {[string is double -strict [%W get]] &&
                [%W get] >= [%W cget -from] &&
                [%W get] <= [%W cget -to]} {
                event generate %W <<NumspinboxChanged>> -data [%W get]
            }
        }

        set fastIncrement {{w scale} {
            set v [format [$w cget -format] \
                       [expr {[$w get] + [$w cget -increment] * $scale}]]
            $w delete 0 end
            $w insert end $v
            event generate $w <<NumspinboxChanged>> -data $v
            return -code break
        }}

        bind Numspinbox <Up>    [list apply $fastIncrement %W  1]
        bind Numspinbox <Down>  [list apply $fastIncrement %W -1]

        bind Numspinbox <Prior> [list apply $fastIncrement %W  10]
        bind Numspinbox <Next>  [list apply $fastIncrement %W -10]

        bind Numspinbox <Control-Prior> [list apply $fastIncrement %W  100]
        bind Numspinbox <Control-Next>  [list apply $fastIncrement %W -100]

        $hull icursor end
    }

    method insert {index str} {
        set allowedChars [list 1 2 3 4 5 6 7 8 9 0]

        if {[$win cget -from] < 0} {
            lappend allowedChars -
        }

        if {[regexp {\.[^0]\d*} [$win cget -format]]} {
            lappend allowedChars .
        }

        if {[string length $str] > 1} {
            # try to paste
            if {[string is double -strict $str]} {
                if {$str < [$hull cget -from]} {
                    $hull set [format [$hull cget -format] [$hull cget -from]]
                } elseif {$str > [$hull cget -to]} {
                    $hull set [format [$hull cget -format] [$hull cget -to]]
                } else {
                    $hull set [format [$hull cget -format] $str]
                }
            }
        } elseif {$str ni $allowedChars} {
            # continue if str is a part of number
        } elseif {$str eq "." && [$win get] eq ""} {
            # "." -> "0."
            $hull set "0."
        } elseif {$str eq "." && [$win get] eq "-"} {
            # "-" -> "-0."
            $hull set "-0."
        } elseif {$str eq "-"} {
            # try to change sign of number
            if {[$win cget -from] < 0} {
                # if allow to input negative numbers
                # switch sign (+-)
                if {[string index [$win get] 0] eq "-"} {
                    $hull delete 0
                } else {
                    $hull insert 0 "-"
                }
            }
        } elseif {[$win get] eq  "0" && [string is digit -strict $str]} {
            # replace leading 0 with new digit
            $hull set $str
        } elseif {[$win get] eq "-0" && [string is digit -strict $str]} {
            # replace leading -0 with new digit
            $hull set -$str
        } else {
            set pos [$win index $index]
            set newText [string range [$win get] 0 $pos-1]$str[string range [$win get] $pos end]

            set width [$hull cget -width]

            if {[$hull cget -from] < 0 && $newText >= 0} {
                incr width -1
            }

            if {[string length $newText] <= $width} {
                if {[string is double -strict $newText]} {
                    regexp {^%(\d*)\.(\d*)f$} [$hull cget -format] -> integerLength fractionLength
                    if {$integerLength ne "" &&
                        [string length [lindex [split $newText .] 0]] > $integerLength} {
                    } elseif {$fractionLength ne "" &&
                              [string length [lindex [split $newText .] 1]] > $fractionLength} {

                    } else {
                        $hull insert $index $str

                        if {[string match {0[1-9]*} [$win get]]} {
                            $hull delete 0
                        } elseif {[string match {-0[1-9]*} [$win get]]} {
                            $hull delete 1
                        }
                    }
                }
            }
        }

        $self CheckRange
    }

    method delete {args} {
        $hull delete {*}$args

        $self CheckRange
    }

    method selection {args} {
        if {[lindex $args 0] in {clear present}} {
            $hull selection {*}$args
        }
    }

    method icursor {index} {
        $hull icursor end
    }

    method CheckRange {} {
        if {[$hull get] < [$hull cget -from]} {
            $self LowValue
        } elseif {[$hull get] > [$hull cget -to]} {
            $self HighValue
        } else {
            $self NormalValue
        }

        $hull icursor end
    }

    method LowValue {} {
        $hull configure -bg #0080ff
        grab set $win
    }

    method HighValue {} {
        $hull configure -bg red
        grab set $win
    }

    method NormalValue {} {
        $hull configure -bg [lindex [$hull configure -bg] 3]
        grab release $win
        event generate $win <<NumspinboxChanged>> -data [$hull get]
    }

    delegate option * to hull; # except -width
    delegate method * to hull
}


if {[info exists argv0] && [file tail [info script]] eq [file tail $argv0]} {
    set value -1000

    numspinbox .numeric -from -10 -to 10 -increment 0.241 -justify right -textvariable value
    label .l -textvariable value

    button .close -text Close -command exit

    pack .numeric .l .close
    bind . <Escape> exit

    raise .
    focus -force [lindex [winfo children .] 0]
}
