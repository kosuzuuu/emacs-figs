My personal init configuratio for Emacs. Feel free to use and tweak it however you like.\
To start using it, clone the repo to your User Emacs directory. (e.g ~/.emacs.d)\
```git clone https://github.com/kosuzuuu/init.el.git```
## Notes
* This config for Emacs is set up mostly for C and C++ development using LSP Mode. If you wish to set up your Emacs for other languages. You can check out the [Languages section from the LSP Mode page.](https://emacs-lsp.github.io/lsp-mode/page/languages/) 
* ```(load-theme 'ef-winter  t)		; Set custom theme```\
The custom theme that i'm using (as of now) requires [ef-themes](https://elpa.gnu.org/packages/ef-themes.html) installed.
* ```(set-face-attribute 'default nil :font "CascadiaMono" :height 100) ; Set custom font```\
I used 'Cascadia Mono' to use as the custom font which require you already have it intalled in your system. You can grab the font through [here.](https://github.com/microsoft/cascadia-code)
