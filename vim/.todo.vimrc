" Nathan's Portable Vim Settings

set nocompatible							" We don't need legacy right?
filetype off

" Pathogen
execute pathogen#infect()

" General Settings
set t_Co=256                                " Enable 256-colour mode
set encoding=utf-8
scriptencoding utf-8
syntax on							    	" Enable syntax highlighting
set showmatch								" Highlights matching brackets
set backspace=2							    " Allows backspace to delete lines
set softtabstop=4							" Allows backspace to delete tabs
set cursorline								" Highlight the current line
set clipboard=unnamedplus                   " Use system clipboard for copy/paste
set wrap                                    " No linewraping
set linebreak
set shortmess=a                             " Shorter messages
set cmdheight=2                             " 2 lines of cmds
set autochdir                               " Auto change dir to todo file dir
set laststatus=0                            " Hide the statusline
set noruler                                 " No ruler
set swapfile
set dir=~/tmp                               " Set swapfiles to tmp directory
autocmd VimEnter * echo ""
cabbrev help tab help

" Smooth Scrolling
:map <C-U> <C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y>
:map <C-D> <C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E>

" Change cursor shape based on mode
if has('unix')
    if has('autocmd')
        au VimEnter,InsertLeave * silent execute '!echo -ne "\e[1 q"' | redraw!
        au InsertEnter,InsertChange *
        \ if v:insertmode == 'i' |
        \   silent execute '!echo -ne "\e[5 q"' | redraw! |
        \ elseif v:insertmode == 'r' |
        \   silent execute '!echo -ne "\e[3 q"' | redraw! |
        \ endif
        au VimLeave * silent execute '!echo -ne "\e[ q"' | redraw!  
    endif
endif

" Theme
colorscheme molokai 
highlight NonText ctermfg=16
if has("unix")
    set guifont=Source_Code_Pro:h13
else
    set guifont=Consolas:h12
endif

" Indent settings
set tabstop=4								" Tab spacing equals 4 spaces
set shiftwidth=4							" Indent by 4 columns
set expandtab								" Use spaces instead of tabs
set shiftround								" Round to the nearest tab
set viewdir=~/vimfiles/view				    " Change vim view default location

" Searching
set hlsearch								" Highlight searches
set noincsearch								" Highlight as you search
set ignorecase								" Make searches not case-sensitive

" Rainbow Parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
au Syntax * RainbowParenthesesLoadChevrons

" Indent Line
let g:indentLine_color_term = 098

" DelimitMate
let delimitMate_expand_cr = 2

" Custom Mappings

let mapleader=','							" Set <leader> to ,
    " Move left/right between windows with Ctrl+H or L
    nnoremap <C-H> <C-W><C-H>
    nnoremap <C-J> <C-W><C-J>
    nnoremap <C-K> <C-W><C-K>
    nnoremap <C-L> <C-W><C-L>

    " Simple move one space right while in insert mode (useful for escaping brackets)
    inoremap <leader>m <Esc>la

    " Reload Vim with edited vimrc
    nmap <leader>ev :e $MYVIMRC<CR> <bar> :loadview<CR>
    nmap <leader>sv :so $MYVIMRC<CR>

    " Toggle Spellcheck
    nnoremap <leader>sp :setlocal spell! spelllang=en_ca<CR>

    " Quick close window/tab
    nnoremap <leader>q :q<CR>

    " Quick indent entire file
    nnoremap <leader>indent gg=G

    " Map Normal mode to nn
    inoremap nn <Esc>

    " Map quick save to ,tt 
    nnoremap <leader>tt <Esc>:w<CR>
    inoremap <leader>tt <Esc>:w<CR>

    " Open new tab / Move to next tab
    nnoremap <C-t> :tabnew<CR>
    nnoremap <C-g> :tabnext<CR>

    " Move cursor naturally if there are line breaks
    nnoremap j gj
    nnoremap k gk

    " Move to beginning and end more easily
    nnoremap H 0
    nnoremap L $

    " Code Completion
    set omnifunc=syntaxcomplete#Complete

    " Close HTML tags
    imap <leader>/ </<C-X><C-O><C-X>

    " Plugin Mappings
        " EasyMotion
        map <Leader>s <Plug>(easymotion-bd-f)
        let g:EasyMotion_smartcase = 1
        " Vim-Commentary
        vmap <Leader>comm <Plug>CommentaryLine

