" configuraciòn neovim - justo manrique
" última actualizaciòn: 01/08/2019
" Configuraciones iniciales
syntax on
set number
set encoding=utf-8
filetype plugin indent on
set nocompatible
set termguicolors
setlocal spell spelllang=es_es

" Instalación de plugins neovim/vim
call plug#begin()
Plug 'lervag/vimtex'
Plug 'sirver/ultisnips'
Plug 'scrooloose/nerdtree'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'sainnhe/gruvbox-material'
Plug 'itchyny/lightline.vim'
Plug 'vim-syntastic/syntastic'
Plug 'kassio/neoterm'
call plug#end()

let g:NERDTreeNodeDelimiter = "\u00a0"

" Configuración de vimtex
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=2
let g:tex_conceal='abdmgs'
set conceallevel=2
let &runtimepath = '~/.config/nvim/plugged/vimtex,' . &runtimepath
let &runtimepath .= ',~/.config/nvim/plugged/vimtex/after'

" Configuración de ultisnips
inoremap <Tab> <c-r>=UltiSnips#ExpandSnippet()<cr>
let g:UltiSnipsExpandTrigger = '<F2>'
let g:UltiSnipsJumpForwardTrigger = '<F2>'
let g:UltiSnipsJumpBackwardTrigger = '<F2>'
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsUsePythonVersion =3 
let g:UltiSnipsSnippetDirectories=[$HOME . "/.config/nvim/ultisnips"]

" Configuración de NERDTree
autocmd VimEnter * NERDTree
autocmd BufEnter * NERDTreeMirror
autocmd VimEnter * wincmd w
autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Configuración de syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_python_checkers = ['pylint','python']
let g:syntastic_r_checkers=['lintr']

" Configuración de colores
colorscheme gruvbox-material

" Configuración de colores lightline
let g:lightline = {'colorscheme' : 'gruvbox_material'}
let g:latex_to_unicode_auto = 1

" Configuración de vim-julia \ language-server
let g:default_julia_version = '1.0'
let g:LanguageClient_autoStart = 1

" Configuración neoterm
nmap gx <Plug>(neoterm-repl-send)
xmap gx <Plug>(neoterm-repl-send)
nmap gcx <Plug>(neoterm-repl-send)
let g:neoterm_autoscroll = 1

" Configuración cuando se abre archivos de julia
au BufReadPost,BufNewFile *.jl bot Tnew | T julia
au BufReadPost,BufNewFile *.R bot Tnew | T R 

