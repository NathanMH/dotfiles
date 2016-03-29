" Nathan's Portable Vim Settings

set nocompatible							" We don't need legacy right?
filetype off

" General Settings

set t_Co=256                                " Enable 256-colour mode
syntax on							    	" Enable syntax highlighting
set number 							    	" Line numbers
set relativenumber                          " Show relative numbers to current
set ruler								    " Show cursor at all times
set showmode								" Shows which mode vim is in (bottom right)
set scrolloff=999                           " Keeps the cursor centered when scrolling
set showmatch								" Highlights matching brackets
set backspace=2								" Allows backspace to delete lines
set softtabstop=4							" Allows backspace to delete tabs
set cursorline								" Highlight the current line
setlocal spell spelllang=en_ca

" Smooth Scrolling
:map <C-U> <C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y>
:map <C-D> <C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E>

" Theme

colorscheme molokai 
set guifont=Source_Code_Pro:h13

" Indent settings

set tabstop=4								" Tab spacing equals 4 spaces
set shiftwidth=4							" Indent by 4 columns
set expandtab								" Use spaces instead of tabs
set shiftround								" Round to the nearest tab
set viewdir=~/vimfiles/view					" Change vim view default location

" Searching

set hlsearch								" Highlight searches
set noincsearch								" Highlight as you search
set ignorecase								" Make searches not case-sensitive
nnoremap <leader>/ :let @/=""<CR>

" Custom Mappings

let mapleader=","							" Set <leader> to ,

    " Move left/right between windows with Ctrl+H or L
    nnoremap <C-L> <C-W><C-L>
    nnoremap <C-H> <C-w><C-H>

    " Autocomplete all brackets and quotes
    inoremap ( ()<Esc>i
    inoremap [ []<Esc>i
    inoremap { {<CR><BS>}<Esc>ko
    inoremap ' ''<Esc>i
    inoremap " ""<Esc>i

    " Simple move one space right while in insert mode (useful for escaping brackets)
    inoremap <leader>m <Esc>la

    " Reload Vim with edited vimrc
    nmap <leader>ev :e $MYVIMRC<CR> <bar> :loadview<CR>
    nmap <leader>sv :so $MYVIMRC<CR>

    " Toggle Spellcheck
    nnoremap <leader>sp :setlocal spell! spelllang=en_ca<CR>

    " Miscellaneous Mappings
        " Map Normal mode to nn
        inoremap nn <Esc>

        " Map quick save to tt 
        nnoremap <leader>tt <Esc>:w<CR>
        inoremap <leader>tt <Esc>:w<CR>

        " Open new tab / Move to next tab
        nnoremap <C-t> :tabnew<CR>
        nnoremap <C-tab> :tabnext<CR>

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

