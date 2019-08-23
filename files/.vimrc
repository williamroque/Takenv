" remove compatiblility with vi
set nocompatible

" detect file type
filetype plugin indent on

" set backspace to normal
set backspace=indent,eol,start

" turn on sytax highlighting
syntax on

" turn on line count
set number

" change leader
let mapleader=","

" spell checking and encoding
set spelllang=en_us
set encoding=utf-8 nobomb

" reload files changed externally
set autoread

" automatically cwd into file directory
set autochdir

" extra margin
set foldcolumn=1

" automatically fold
set foldmethod=syntax
set nofoldenable

" status line
set laststatus=2
set ttimeoutlen=50
let g:airline_theme='base16'

let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" buffer is not necessarily written to disk
set hidden

set wildmenu " tab for autocompletions in command mode
set wildchar=<TAB>
set wildmode=list:longest

" search
set gdefault " RE default global
set magic " extended RE
set incsearch " searches incrementally
set hlsearch " highlight search
set ignorecase smartcase " searches ignore case unless upper case
" stops highlight until next search
nnoremap <silent> \ :silent nohls<CR>

" respect columns
set nostartofline

" actively shows commands
set showcmd

" indentation
set autoindent smartindent
set shiftwidth=4 " indentation width
set softtabstop=4 " generated spaces for tab
set expandtab " expand tabs to spaces
set tabstop=4 " tab number of space
set smarttab

set formatoptions=qrn1

" general auto comment disable
au FileType vim set fo-=c fo-=r fo-=o
au FileType javascript set fo-=c fo-=r fo-=o

" disable swap files when editting crontab
autocmd filetype crontab setlocal nobackup nowritebackup

" new tab
nmap <Leader>te :tabedit 

" close tab
nmap <Leader>tc :tabclose<CR>

" only keep current tab
nmap <silent> <Leader>to :tabonly<CR>

" autoesc
inoremap jj <Esc>j
inoremap jk <Esc>
inoremap kk <Esc>k

" faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" move line up/down
vnoremap <silent> <S-Down> :m '>+1<CR>gv=gv
vnoremap <silent> <S-Up> :m '<-2<CR>gv=gv

" open file
noremap <silent> <Leader>op :!open %<CR>

" toggle spelling hints
nnoremap <silent> <Leader>ts :set spell!<CR>

" split window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" close window
noremap <silent> <leader>q :silent clo<CR>

" resize splits
nnoremap <silent> <Leader>sh+ :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>sh- :exe "resize " . (winheight(0) * 2/3)<CR>
nnoremap <silent> <Leader>sv+ :exe "vertical resize " . (winwidth(0) * 3/2)<CR>
nnoremap <silent> <Leader>sv- :exe "vertical resize " . (winwidth(0) * 2/3)<CR>

" split horizontally
nmap <silent> <Leader>sph :split<CR>

" split vertically
nmap <silent> <Leader>spv :vsplit<CR>

" edit file
nmap <Leader>oe :e 

" jump to next error
nmap <silent> <Leader>ne :ALENext<CR>

" height of command displayer
set cmdheight=2

" remap o and O in order to allow numbers
nnoremap o o<Esc>i
nnoremap O O<Esc>i

" open newline without leaving
nmap zj o<Esc>
nmap zk O<Esc>

" map ; to :
nmap ; :

" bar shaped cursor in insert mode
let &t_SI = "\<Esc>]1337;CursorShape=1\x7"
let &t_EI = "\<Esc>]1337;CursorShape=0\x7"

set cursorline " highlight current line
set splitbelow splitright " how to split new windows

set scrolloff=5 " start scrolling 5 lines before bottom
set sidescrolloff=7 " start scrolling 7 lines before right limit
set sidescroll=1 " minimal number to scroll horizontally

" plugins
if has('nvim')
    if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
        !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    endif
else
    if empty(glob('~/.vim/autoload/plug.vim'))
        !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    endif
endif

call plug#begin('~/.vim/plugged')

" status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" markdown
Plug 'junegunn/goyo.vim'
" Plug 'junegunn/limelight.vim'

" HTML
Plug 'alvan/vim-closetag'
Plug 'mattn/emmet-vim'
Plug 'jaxbot/browserlink.vim'

" theme
" Plug 'chriskempson/base16-vim'
Plug 'morhetz/gruvbox'

" editing
Plug 'tpope/vim-surround'
Plug 'w0rp/ale', { 'on':  'ALEToggle' }
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

" language
Plug 'sheerun/vim-polyglot'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

" navigation
" Plug 'scrooloose/nerdtree'
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Plug 'junegunn/fzf.vim'
Plug 'easymotion/vim-easymotion'

" version control
Plug 'airblade/vim-gitgutter'

" misc
if has('nvim')
    Plug 'raghur/vim-ghost', {'do': ':GhostInstall'}
endif

nmap <Leader>at :ALEToggle<CR>

call plug#end()
nmap <Leader>pi :PlugInstall<CR>

" markdown environment
let g:limelight_conceal_ctermfg = 'gray'
function TgGoyoMD()
    set spell
    call deoplete#custom#option('auto_complete', v:false)
    Goyo
endfunction

autocmd BufNewFile,BufRead *.md call TgGoyoMD()
nmap <silent> <Leader>md :call TgGoyoMD()<CR>

" deoplete
let g:deoplete#enable_at_startup = 1

" linting
" let g:ale_javascript_eslint_use_global = 1

" colorscheme

" gruvbox
set bg=dark
let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
let g:gruvbox_italic=0
colorscheme gruvbox

" NERDTree
" when vim starts on a directory
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

" close vim when NT is the only window left
" autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" map for NT toggle
" map <C-o> :NERDTreeToggle<CR>

" change windows-map
nmap <Leader>w <C-w><C-w>

" emmet
let g:user_emmet_mode='ni'
let g:user_emmet_leader_key="§"

" save files quickly
map <Leader>f :w<CR>

" open command prompt
map <Leader>c :!

" npm start
map <Leader>ns :!npm start<CR>

" save and close file
map <Leader>x :x<CR>

" quit file
map <Leader>e :q!<CR>

" set hybrid line numbers
set number relativenumber
set nu rnu

" page down map
nmap <S-j> <C-e>

" page up map
nmap <S-k> <C-y>

" access clipboard
" nmap <Leader>v "*
" map <Leader>v "*

" automatically access clipboard on yank and paste
set clipboard=unnamed

" access black hole
nmap <Leader>b "_

" disable error bell
set noeb vb t_vb=
au GUIEnter * set vb t_vb=

" language client neovim
let g:LanguageClient_autoStart = 1
nnoremap <leader>lcs :LanguageClientStart<CR>

let g:LanguageClient_serverCommands = {
    \ 'python': ['pyls'],
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'go': ['go-langserver'] }

noremap <silent> HH :call LanguageClient_textDocument_hover()<CR>
noremap <silent> ZZ :call LanguageClient_textDocument_definition()<CR>
noremap <silent> RR :call LanguageClient_textDocument_rename()<CR>
noremap <silent> SS :call LanugageClient_textDocument_documentSymbol()<CR>

" macro keymap
nnoremap <Space> @q
vnoremap <Space> :norm @q<CR>

" replace all matching word under cursor
nnoremap <Leader>s :%s/\<<C-r><C-w>\>/

" preview markdown
nnoremap <Leader>pv :!~/cbin/preview<CR>

" read file
nmap <Leader>r :r 
