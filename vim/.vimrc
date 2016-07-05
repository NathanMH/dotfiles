" Nathan's Portable Vim Settings

set nocompatible							" We don't need legacy right?
filetype off

" Pathogen & Plugins

execute pathogen#infect()

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
set nowrap
setlocal spell spelllang=en_ca

" Smooth Scrolling
:map <C-U> <C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y><C-Y>
:map <C-D> <C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E><C-E>

" Cursor shape 

if has("autocmd")
  au InsertEnter * silent execute "!sed -i.bak -e 's/TERMINAL_CURSOR_SHAPE_BLOCK/TERMINAL_CURSOR_SHAPE_UNDERLINE/' ~/.config/xfce4/terminal/terminalrc"                                                                                          
  au InsertLeave * silent execute "!sed -i.bak -e 's/TERMINAL_CURSOR_SHAPE_UNDERLINE/TERMINAL_CURSOR_SHAPE_BLOCK/' ~/.config/xfce4/terminal/terminalrc"                                                                                          
  au VimLeave * silent execute "!sed -i.bak -e 's/TERMINAL_CURSOR_SHAPE_UNDERLINE/TERMINAL_CURSOR_SHAPE_BLOCK/' ~/.config/xfce4/terminal/terminalrc"  
endif

" Theme

colorscheme molokai 
set guifont=Source_Code_Pro:h13

" Indent settings

set tabstop=4								" Tab spacing equals 4 spaces
set shiftwidth=4							" Indent by 4 columns
set expandtab								" Use spaces instead of tabs
set shiftround								" Round to the nearest tab
set viewdir=~/vimfiles/view					" Change vim view default location

" Searching/EasyMotion
nmap <Leader><Leader>s <Plug>(easymotion-bd-f)
let g:EasyMotion_smartcase = 1

set hlsearch								" Highlight searches
set noincsearch								" Highlight as you search
set ignorecase								" Make searches not case-sensitive
nnoremap <leader>/ :let @/=""<CR>

" Netrw

let g:netrw_banner = 0
let g:netrw_list_hide = '.*\.swp$,\.DS_Store'
let g:netrw_liststyle = 3
let g:netrw_sort_sequence = '[\/]$'
let g:netrw_sort_options = 'i'
let g:netrw_altv = 1
let g:netrw_winsize = 25
let g:netrw_browse_split = 4

" Custom Mappings

let mapleader=","							" Set <leader> to ,

    " Open Netrw
    nnoremap <leader>ne :Vexplore ~/Documents/<CR>

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

    " Quick close window/tab
    nnoremap <leader>q :q<CR>

    " Miscellaneous Mappings
        " Map Normal mode to nn
        inoremap nn <Esc>

        " Map quick save to tt 
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

