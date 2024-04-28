gsettings set org.gnome.desktop.interface gtk-theme "Adwaita"
gsettings set org.gnome.desktop.interface clock-show-weekday true
gsettings set org.gnome.desktop.interface enable-hot-corners false
gsettings set org.gnome.desktop.privacy remember-recent-files false
gsettings set org.gnome.system.location enabled true
gsettings set org.gnome.desktop.datetime automatic-timezone true
# window
gsettings set org.gnome.desktop.wm.preferences mouse-button-modifier "<Super>"
gsettings set org.gnome.desktop.wm.preferences resize-with-right-button true
gsettings set org.gnome.mutter attach-modal-dialogs false
# touchpad
gsettings set org.gnome.desktop.peripherals.touchpad natural-scroll false
gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click true
gsettings set org.gnome.desktop.peripherals.touchpad tap-and-drag false
# input
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('ibus', 'rime')]"
gsettings set org.gnome.desktop.input-sources per-window true
# extension
gsettings set org.gnome.shell disable-user-extensions false
gnome-extensions enable dash-to-panel@jderose9.github.com
gnome-extensions enable Vitals@CoreCoding.com
gnome-extensions enable appindicatorsupport@rgcjonas.gmail.com
gnome-extensions enable hide-universal-access@akiirui.github.io
gnome-extensions enable caffeine@patapon.info
# dash to panel
gsettings set org.gnome.shell.extensions.dash-to-panel panel-positions '{"0":"TOP"}'
gsettings set org.gnome.shell.extensions.dash-to-panel dot-style-unfocused 'DASHES'
gsettings set org.gnome.shell.extensions.dash-to-panel dot-style-focused 'DASHES'
gsettings set org.gnome.shell.extensions.dash-to-panel trans-use-custom-bg true
gsettings set org.gnome.shell.extensions.dash-to-panel trans-use-custom-opacity true
gsettings set org.gnome.shell.extensions.dash-to-panel trans-use-dynamic-opacity true
gsettings set org.gnome.shell.extensions.dash-to-panel trans-use-custom-gradient true
gsettings set org.gnome.shell.extensions.dash-to-panel show-favorites false
gsettings set org.gnome.shell.extensions.dash-to-panel animate-appicon-hover true
gsettings set org.gnome.shell.extensions.dash-to-panel overview-click-to-exit true
gsettings set org.gnome.shell.extensions.dash-to-panel hide-overview-on-startup true
gsettings set org.gnome.shell.extensions.dash-to-panel stockgs-keep-dash true
gsettings set org.gnome.shell.extensions.dash-to-panel panel-element-positions '{"0":[{"element":"showAppsButton","visible":false,"position":"stackedTL"},{"element":"activitiesButton","visible":true,"position":"stackedTL"},{"element":"leftBox","visible":true,"position":"stackedTL"},{"element":"taskbar","visible":true,"position":"stackedTL"},{"element":"centerBox","visible":true,"position":"stackedBR"},{"element":"dateMenu","visible":true,"position":"centerMonitor"},{"element":"rightBox","visible":true,"position":"stackedBR"},{"element":"systemMenu","visible":true,"position":"stackedBR"},{"element":"desktopButton","visible":false,"position":"stackedBR"}]}'
gsettings set org.gnome.shell favorite-apps "['org.gnome.Nautilus.desktop', 'firefox.desktop', 'org.gnome.DiskUtility.desktop', 'io.github.celluloid_player.Celluloid.desktop', 'org.gnome.baobab.desktop', 'com.obsproject.Studio.desktop', 'app.drey.Warp.desktop', 'io.gitlab.adhami3310.Impression.desktop', 'com.github.finefindus.eyedropper.desktop', 'com.vixalien.sticky.desktop', 'com.github.johnfactotum.Foliate.desktop', 'de.haeckerfelix.Fragments.desktop']"
# vitals
gsettings set org.gnome.shell.extensions.vitals hot-sensors "['__network-rx_max__', '__temperature_max__']"
gsettings set org.gnome.shell.extensions.vitals hide-icons true
gsettings set org.gnome.shell.extensions.vitals fixed-widths true
# caffeine
gsettings set org.gnome.shell.extensions.caffeine restore-state true
# default app
gio mime inode/directory org.gnome.Nautilus.desktop
