;; https://askubuntu.com/questions/393400/is-it-possible-to-have-different-dpi-configurations-for-two-different-screens

;; screen
(start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 2880x1800 --pos 3840x0 --rotate normal --output DP1 --off --output DP2 --off --output HDMI1 --off --output HDMI2 --off --output HDMI3 --mode 2560x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off")

;; Middlebush
(start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 2880x1800 --pos 0x0 --rotate normal --output DP1 --off --output DP2 --off --output HDMI1 --off --output HDMI2 --off --output HDMI3 --scale 1.5x1.67 --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off")

;; Strick
(start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 2880x1800 --pos 0x0 --rotate normal --output DP1 --off --output DP2 --off --output HDMI1 --off --output HDMI2 --off --output HDMI3 --scale 1.5x1.5 --mode 1920x1200 --pos 0x0 --rotate normal --output VIRTUAL1 --off")

;; (start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 2880x1800 --pos 0x0 --rotate normal --output DP1 --off --output DP2 --off --output HDMI1 --off --output HDMI2 --off --output HDMI3 --scale 1.5x1.5 --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off")

;; test scaling
(start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 2880x1800 --pos 5440x0 --rotate normal --output DP1 --off --output DP2 --off --output HDMI1 --off --output HDMI2 --off --output HDMI3 --scale 2x2 --mode 2560x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off")
