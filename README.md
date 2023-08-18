<div align="center">

# Zhenhua Emacs Config

| ![desktop](./screenshots/desktop.png) | ![agenda](./screenshots/agenda.png) |
|:-------------------------------------:|:-----------------------------------:|
| ![web](./screenshots/web.png)         | ![ide](./screenshots/ide.png)       |

</div>

---

### Table of Contents

1.  [Prerequisite](#prerequisite)
2.  [Installation](#installation)
3.  [Main Packages](#main-packages)

# Prerequisite

-   emacs-29
-   Noto Sans Mono: https://fonts.google.com/download?family=Noto%20Sans%20Mono
-   Noto Sans Mono CJK SC: https://github.com/googlefonts/noto-cjk/raw/main/Sans/Variable/TTF/Mono/NotoSansMonoCJKsc-VF.ttf
-   Note Emoji: https://fonts.google.com/noto/specimen/Noto+Emoji
-   hack: https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip
-   EB Garamond: https://fonts.google.com/download?family=EB%20Garamond

# Installation

-   clone this reposition to your home folder (~).
-   change folder name to `.emacs.d`. (adding a dot in the front)
-   `cp .emacs.d/fonts/* ~/.fonts && fc-cache -f -v`

# Main Packages

- vertico + marginalia + consult
- company-mode + company-posframe
- lsp-mode + lsp-ui
- ess + polymode + stan-mode
- python-mode + conda + code-cells
- auctex + reftex

