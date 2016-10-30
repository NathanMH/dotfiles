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
set number 							    	" Line numbers
set relativenumber                          " Show relative numbers to current
set ruler								    " Show cursor at all times
set showmode								" Shows which mode vim is in (bottom right)
set scrolloff=999                           " Keeps the cursor centered when scrolling
set showmatch								" Highlights matching brackets
set backspace=2							    " Allows backspace to delete lines
set softtabstop=4							" Allows backspace to delete tabs
set cursorline								" Highlight the current line
set clipboard=unnamedplus                   " Use system clipboard for copy/paste
set nowrap                                  " No linewraping
set noshowmode                              " Don't show mode (airline)
set laststatus=2                            " Start with lightline/airline/powerline active
cabbrev help tab help

if has("win32")
    let $TMP="C:/tmp"
endif

if has("gui_running")
    set guioptions-=m                  " Remove menu bar
    set guioptions-=T                  " Remove toolbar
    set guioptions-=r                  " Remove scrollbar
    set guioptions-=L                  " Remove left scrollbar
endif

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
if has("unix")
    set guifont=Source_Code_Pro:h13
else
    set guifont=Consolas:h12
endif

if &term =~ '256color'
    " Disable Background Color Erase (BCE)
    set t_ut=
endif

" Indent settings
set tabstop=4								" Tab spacing equals 4 spaces
set shiftwidth=4							" Indent by 4 columns
set expandtab								" Use spaces instead of tabs
set shiftround								" Round to the nearest tab
set autoindent                              " Auto indent
set viewdir=~/vimfiles/view				    " Change vim view default location

" Searching
set hlsearch								" Highlight searches
set noincsearch								" Highlight as you search
set ignorecase								" Make searches not case-sensitive

"Plugins
" Calendar
let g:calendar_google_calendar = 1
let g:calendar_google_task = 1

" Netrw
let g:netrw_banner = 0
let g:netrw_list_hide = '.*\.swp$,\.DS_Store'
let g:netrw_liststyle = 3
let g:netrw_sort_sequence = '[\/]$'
let g:netrw_sort_options = 'i'
let g:netrw_altv = 1
let g:netrw_winsize = 25
let g:netrw_browse_split = 4

" Rainbow Parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
au Syntax * RainbowParenthesesLoadChevrons

" Lightline/Airline/Powerline - Keep this simple (no patched fonts)
let g:lightline = {
    \   'colorscheme': 'landscape',
    \   'active': {
    \       'left': [['mode', 'paste'], ['readonly', 'absolutepath', 'modified']],
    \       'right': [['percent'], ['syntastic'], ['filetype']]
    \   },
    \
    \   'component_function': {
    \       'syntastic': 'SyntasticStatuslineFlag',
    \   },
    \
    \   'separator': {'left': '', 'right': ''},
    \   'subseparator': {'left': '', 'right': ''}
    \}

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_loc_list_height = 4
let g:syntastic_vim_checkers = ['vint']
let g:syntastic_python_checkers = ['pylint', 'flake8']
let g:syntastic_python_python_exec = '/usr/bin/python3.4'
let g:syntastic_error_symbol = '!'
let g:syntastic_warning_symbol = '>'

" YouCompleteMe
let g:ycm_seed_identifiers_with_syntax = 1        

" UltiSnips
" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" Indent Line
let g:indentLine_color_term = 098

" DelimitMate
let delimitMate_expand_cr = 2

" Vim-Rooter
let g:rooter_silent_chdir = 1

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

    " Map Esc to nn
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
        " Open Netrw
        if has("unix")
            nnoremap <leader>ne :Vexplore ~/Documents/<CR>
        else
            nnoremap <leader>ne :Vexplore C:/Documents/<CR>
        endif
        " Syntastic On/Off
        nnoremap <S-s> :SyntasticToggleMode<CR>
        " Keybind for Calendar
        nnoremap <Leader>cal :Calendar<CR>
        " UltiSnips
        nnoremap <Leader><Tab> <esc>:call UltiSnips#ExpandSnippet()<cr>

