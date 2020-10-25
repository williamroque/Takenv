" remove compatiblility with vi
set nocompatible

" detect file type
filetype plugin indent on

" set backspace to normal
set backspace=indent,eol,start

" turn on sytax highlighting
syntax on

" removes delays
set updatetime=300

" change leader
let mapleader=','

" spell checking and encoding
set spelllang=en_us
set encoding=utf-8 nobomb

" reload files changed externally
set autoread

" automatically change cwd to file directory
set autochdir

" extra margin
" set foldcolumn=1

" automatically fold
set foldmethod=syntax
set nofoldenable

" status line
set laststatus=2
set ttimeoutlen=50
let g:airline_theme='base16'

let g:airline_powerline_fonts=1

if !exists('g:airline_symbols')
    let g:airline_symbols={}
endif

" unicode symbols
let g:airline_left_sep='»'
let g:airline_left_sep='▶'
let g:airline_right_sep='«'
let g:airline_right_sep='◀'
let g:airline_symbols.linenr='␊'
let g:airline_symbols.linenr='␤'
let g:airline_symbols.linenr='¶'
let g:airline_symbols.branch='⎇'
let g:airline_symbols.paste='ρ'
let g:airline_symbols.paste='Þ'
let g:airline_symbols.paste='∥'
let g:airline_symbols.whitespace='Ξ'

" airline symbols
let g:airline_left_sep=''
let g:airline_left_alt_sep=''
let g:airline_right_sep=''
let g:airline_right_alt_sep=''
let g:airline_symbols.branch=''
let g:airline_symbols.readonly=''
let g:airline_symbols.linenr=''

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
inoremap jk <Esc>

" faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

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
noremap <silent> <leader>qu :silent clo<CR>

" resize splits
nnoremap <silent> <Leader>sh+ :exe 'resize ' . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>sh- :exe 'resize ' . (winheight(0) * 2/3)<CR>
nnoremap <silent> <Leader>sv+ :exe 'vertical resize ' . (winwidth(0) * 3/2)<CR>
nnoremap <silent> <Leader>sv- :exe 'vertical resize ' . (winwidth(0) * 2/3)<CR>
nnoremap <silent> <Leader>seq <C-w>=

" split horizontally
nmap <silent> <Leader>sph :split<CR>

" split vertically
nmap <silent> <Leader>spv :vsplit<CR>

" edit file
nmap <Leader>oe :e 

" height of command displayer
set cmdheight=2

" remap o and O in order to allow numbers
nnoremap o o<Esc>i
nnoremap O O<Esc>i

" open newline without leaving command mode
nmap zj o<Esc>
nmap zk O<Esc>

" map ; to :
nmap ; :

" bar shaped cursor in insert mode
let &t_SI='\<Esc>]1337;CursorShape=1\x7'
let &t_EI='\<Esc>]1337;CursorShape=0\x7'

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

" HTML
Plug 'mattn/emmet-vim'

" theme
Plug 'morhetz/gruvbox'

" editing
Plug 'tpope/vim-surround'
Plug 'dense-analysis/ale'

" language
Plug 'sheerun/vim-polyglot'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" navigation
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'easymotion/vim-easymotion'
Plug 'bkad/CamelCaseMotion'

" version control
Plug 'airblade/vim-gitgutter'

call plug#end()

" ALE
let g:ale_fixers = {
    \ '*': ['remove_trailing_lines', 'trim_whitespace'],
    \ 'javascript': ['eslint']
  \ }
let g:ale_javascript_eslint_suppress_missing_config = 1

nmap <Leader>pi :PlugInstall<CR>

" markdown environment
function TgGoyoMD()
    set spell
    Goyo
endfunction

autocmd BufNewFile,BufRead *.md call TgGoyoMD()
nmap <silent> <Leader>md :call TgGoyoMD()<CR>

" toggle spellcheck language 
function ToggleLang()
    set spell
    if &spelllang == 'en_us'
        set spelllang=pt_br
    else
        set spelllang=en_us
    endif
endfunction
nmap <silent> <Leader>tl :call ToggleLang()<CR>

" language server

set shortmess+=c

" <c-space> for completion
if has('nvim')
    inoremap <silent><expr> <c-space> coc#refresh()
else
    inoremap <silent><expr> <c-@> coc#refresh()
endif

" use enter to confirm and format on enter
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" show all diagnostics
nnoremap <silent><nowait> <Leader>ca :<C-u>CocList diagnostics<cr>

" manage extensions
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>

" smart code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" show documentation
nnoremap <silent> <Leader>doc :call <SID>show_documentation()<CR>

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    elseif (coc#rpc#ready())
        call CocActionAsync('doHover')
    else
        execute '!' . &keywordprg . ' ' . expand('<cword>')
    endif
endfunction

" highlight symbol
autocmd CursorHold * silent call CocActionAsync('highlight')

" rename symbol
nmap <leader>rn <Plug>(coc-rename)

" format selection
xmap for <Plug>(coc-format-selected)
nmap for <Plug>(coc-format-selected)

" format document
command! -nargs=0 Format :call CocAction('format')

" something to do with signature
augroup mygroup
  autocmd!
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" autofix
nmap <leader>qf <Plug>(coc-fix-current)

" codeaction
nmap <leader>ac  <Plug>(coc-codeaction)

" function and class objects
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" colorscheme
set bg=dark
let &t_8f='\<Esc>[38;2;%lu;%lu;%lum'
let &t_8b='\<Esc>[48;2;%lu;%lu;%lum'
let g:gruvbox_italic=1
let g:gruvbox_italicize_strings=1
let g:gruvbox_contrast_dark='hard'
colorscheme gruvbox
let g:gitgutter_set_sign_backgrounds=1
let g:gitgutter_sign_removed='-'
highlight Normal ctermbg=NONE
highlight SignColumn ctermbg=NONE
highlight CursorLineNR ctermbg=NONE
highlight GitGutterAdd ctermfg=2
highlight GitGutterChange ctermfg=3
highlight GitGutterDelete ctermfg=1
highlight CocWarningSign ctermbg=NONE
highlight CocErrorSign ctermbg=NONE
highlight CocFloating ctermbg=0
highlight ALEWarningSign ctermbg=NONE
highlight ALEErrorSign ctermbg=NONE
highlight link CocWarningSign GruvboxYellow
highlight link CocErrorSign GruvboxRed
highlight link ALEWarningSign GruvboxYellow
highlight link ALEErrorSign GruvboxRed

" change windows
nmap <Leader>w <C-w><C-w>

" emmet
let g:user_emmet_mode='ni'
let g:user_emmet_leader_key='§'

" save files quickly
map <Leader>f :w<CR>

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

" CamelCaseMotion
nmap <silent> w <Plug>CamelCaseMotion_w
nmap <silent> b <Plug>CamelCaseMotion_b
nmap <silent> e <Plug>CamelCaseMotion_e
nmap <silent> ge <Plug>CamelCaseMotion_ge
xmap <silent> iw <Plug>CamelCaseMotion_iw

" automatically access clipboard on yank and paste
set clipboard=unnamed

" access black hole
nmap <Leader>b "_

" disable error bell
set noeb vb t_vb=
au GUIEnter * set vb t_vb=

" macro keymap
nnoremap <Space> @q
vnoremap <Space> :norm @q<CR>

" replace all matching word under cursor
nnoremap <Leader>s :%s/\<<C-r><C-w>\>/

" preview markdown
nnoremap <Leader>pv :!~/cbin/preview<CR>

" file navigation
nnoremap <Leader>of :Files 
