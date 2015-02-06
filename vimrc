" Nathan's Portable Vim Settings

set nocompatible								" We don't need legacy right?
filetype off

" Pathogen
execute pathogen#infect()
filetype plugin indent on

" Plugin Settings

    " SuperTab
let g:SuperTabDefaultCompletionType = "<c-n>"

    " NERDTree
autocmd vimenter * NERDTreeFromBookmark Programming
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q
let NERDTreeShowLineNumbers=1
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=0

    " EasyMotion
nmap s <Plug>(easymotion-s)
let g:EasyMotion_use_upper = 1
let g:EasyMotion_smartcase = 1
let g:EasyMotion_use_smartsign_us = 1

" General Settings

set t_Co=256 								" Enable 256-colour mode
syntax on									" Enable syntax highlighting
set number 									" Line numbers
set relativenumber							" Show relative numbers to current
set ruler									" Show cursor at all times
set showmode								" Shows which mode vim is in (bottom right)
set scrolloff=999                           " Keeps the cursor centered when scrolling
set showmatch                               " Highlights matching brackets
set backspace=2                             " Allows backspace to delete lines
set softtabstop=4                           " Allows backspace to delete tabs
set cursorline                              " Highlight the current line
setlocal spell spelllang=en_ca

" Theme

colorscheme monokai 
set guifont=Consolas:h11:cANSI

" Indent settings

set tabstop=4								" Tab spacing equals 4 spaces
set shiftwidth=4							" Indent by 4 columns
set expandtab								" Use spaces instead of tabs
set shiftround								" Round to the nearest tab
set viewdir=~/vimfiles/view                 " Change vim view default location
autocmd BufWrite *.* mkview!                " Save folds on window close
autocmd BufRead *.* loadview                " Load folds on window init

" Searching

set nohlsearch								" Don't continue to highlight searches
set incsearch								" Highlight as you search
set ignorecase								" Make searches not case-sensitive

" Custom Mappings

let mapleader=","                           " Set <leader> to ,

    " Move between windows with Ctrl+HJKL
    nnoremap <C-J> <C-W><C-J>                  
    nnoremap <C-K> <C-W><C-K>
    nnoremap <C-L> <C-W><C-L>
    nnoremap <C-H> <C-W><C-H>

    " Change windows orientation
    nnoremap <leader>wl <C-W>H
    nnoremap <leader>wd <C-W>J
    nnoremap <leader>wu <C-W>K
    nnoremap <leader>wr <C-W>L

    " Set extreme focus mode to , + l
    noremap <leader>l :Limelight!! <bar> :Goyo<CR> <bar> <Esc>:setlocal spell! spelllang=en_ca<CR>

    " Autocomplete all brackets
    inoremap ( ()<Esc>i
    inoremap [ []<Esc>i
    inoremap { {<CR><BS>}<Esc>ko
    inoremap ' ''<Esc>i
    inoremap " ""<Esc>i

   inoremap <leader>m <Esc>la

    " Reload Vim with edited vimrc
    nmap <leader>ev :e $MYVIMRC<CR> <bar> :loadview<CR>
    nmap <leader>sv :so $MYVIMRC<CR>

    " Toggle Spellcheck
    nnoremap <leader>sp :setlocal spell! spelllang=en_ca<CR>

    " Miscellaneous Mappings
        " Toggle NERDTree with , + ne
        nnoremap <leader>ne :NERDTreeFromBookmark Programming<CR>
        " Map Normal mode to nn
        inoremap nn <Esc>
        " Map quick save to tt 
        nnoremap <leader>tt <Esc>:w<CR>
        inoremap <leader>tt <Esc>:w<CR>
        " Get out of brackets quickly with , + m
        inoremap <leader>m <Esc>la
        " Open/close fold 
        nnoremap <leader>oo zo
        nnoremap <leader>cc zc
        nnoremap <leader>OO zR
        nnoremap <leader>CC zM

        " Move to next tab
        nnoremap <C-tab> :tabnext<CR>

        " Run Java file if eclim is running
        " nnoremap <leader>jj :Java<CR>

" Code Completion

let g:SuperTabDefaultCompletionType = 'context'
let g:EclimCompletionMethod = 'omnifunc'

