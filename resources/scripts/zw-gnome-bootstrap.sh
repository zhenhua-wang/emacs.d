cd ~
# yay
sudo pacman -S --needed git base-devel cmake inetutils
git clone https://aur.archlinux.org/yay.git
cd ~/yay && makepkg -si && cd ~

# fonts
yay -S noto-fonts noto-fonts-cjk noto-fonts-emoji

# emacs
mv ~/emacs.d ~/.emacs.d
yay -S emacs aspell aspell-en
emacs -Q --batch --eval "(require 'org)" --eval '(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))' --eval '(defun zw/org-babel-tangle-linux (path) (if (eq system-type (intern "gnu/linux")) path "no"))' --eval '(defun zw/org-babel-tangle-not-exist (path) (if (file-exists-p path) "no" path))' --eval '(org-babel-tangle-file "~/.emacs.d/resources/OrgFiles/dotfiles.org")'

# gnome core
yay -S gdm gnome-shell gnome-control-center gnome-keyring gnome-tweaks networkmanager xdg-desktop-portal-gnome xdg-user-dirs gst-plugins-good power-profiles-daemon switcheroo-control flatpak malcontent iio-sensor-proxy networkmanager-openconnect
sudo systemctl enable gdm.service -f
# gnome extra
yay -S gnome-themes-extra gnome-browser-connector gnome-shell-extension-appindicator gnome-shell-extension-caffeine gnome-shell-extension-vitals gnome-shell-extension-dash-to-dock gnome-shell-extension-blur-my-shell xcursor-breeze ibus-rime rime-ice-git nautilus gnome-calculator gnome-disk-utility baobab loupe evince

# keyd
yay -S keyd-git
sudo systemctl enable keyd && sudo systemctl start keyd
sudo usermod -aG keyd $USER
if [ ! -d ~/.local/share/gnome-shell/extensions/ ]; then
    mkdir ~/.local/share/gnome-shell/extensions/
fi
ln -s /usr/share/keyd/gnome-extension-45 ~/.local/share/gnome-shell/extensions/keyd
keyd-application-mapper -d

# bluetooth
sudo systemctl enable bluetooth.service

# statistics
yay -S r gcc-fortran pandoc texlive-core texlive-latexextra texlive-fontsrecommended texlive-binextra texlive-mathscience texlive-plaingeneric texlive-bibtexextra miniconda3

# zsh
yay -S zsh && chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
echo -e '[ "$TERM" = "xterm-kitty" ] && alias ssh="kitty +kitten ssh"' >> ~/.zshrc

# config git
git config --global user.name "Zhenhua Wang"
git config --global user.email "wang_zhenhua1996@icloud.com"
git config --global credential.helper store

# rime
ln -s ~/.local/share/fcitx5/rime/default.custom.yaml ~/.config/ibus/rime/default.custom.yaml

# essential desktop
yay -S firefox kitty htop neofetch ripgrep obs-studio mpv celluloid yt-dlp streamlink file-roller exfat-utils

# flatpak applications
flatpak install com.github.johnfactotum.Foliate de.haeckerfelix.Fragments io.gitlab.adhami3310.Impression app.drey.Warp com.github.finefindus.eyedropper com.vixalien.sticky

# config
bash ~/.emacs.d/resources/scripts/zw-gnome-config.sh
bash ~/.emacs.d/resources/scripts/zw-gnome-keymap.sh
