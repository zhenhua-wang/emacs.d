cd ~
# yay
sudo pacman -S --needed git base-devel cmake inetutils
git clone https://aur.archlinux.org/yay.git
cd ~/yay && makepkg -si && cd ~

# zw-emacs
mv ~/emacs.d ~/.emacs.d
mkdir ~/.fonts
cp ~/.emacs.d/fonts/* ~/.fonts && fc-cache -fv
# compile emacs
yay -S libxpm libtiff giflib aspell aspell-en
git clone git://git.sv.gnu.org/emacs.git

# core
yay -S gdm gnome-shell gnome-control-center gnome-keyring gnome-tweaks networkmanager xdg-desktop-portal-gnome xdg-user-dirs gst-plugins-good power-profiles-daemon switcheroo-control flatpak malcontent iio-sensor-proxy networkmanager-openconnect
sudo systemctl enable gdm.service -f
# extra
yay -S gnome-themes-extra gnome-browser-connector gnome-shell-extension-dash-to-dock gnome-shell-extension-blur-my-shell gnome-shell-extension-appindicator gnome-shell-extension-hide-universal-access gnome-shell-extension-caffeine gnome-shell-extension-vitals xcursor-breeze ibus-rime nautilus gnome-calculator gnome-disk-utility baobab loupe evince
# keyd
yay -S keyd-git
sudo systemctl enable keyd && sudo systemctl start keyd

# bluetooth
sudo systemctl enable bluetooth.service
# statistics
yay -S r gcc-fortran pandoc texlive-core texlive-latexextra texlive-fontsrecommended texlive-binextra texlive-mathscience texlive-plaingeneric texlive-bibtexextra miniconda3
# zsh
yay -S zsh && chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# config git
git config --global user.name "Zhenhua Wang"
git config --global user.email "wang_zhenhua1996@icloud.com"
git config --global credential.helper store
# essential desktop
yay -S firefox kitty htop neofetch ripgrep obs-studio mpv celluloid yt-dlp streamlink file-roller exfat-utils
# flatpak applications
flatpak install com.github.johnfactotum.Foliate de.haeckerfelix.Fragments io.gitlab.adhami3310.Impression app.drey.Warp com.github.finefindus.eyedropper
