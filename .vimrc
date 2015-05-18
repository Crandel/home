execute pathogen#infect()
" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %
set nocompatible
syntax on

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

" Uncomment the following to have Vim jump to the last position when
" reopening a file
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
"if has("autocmd")
"  filetype plugin indent on
"endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showcmd         " Show (partial) command in status line.
set showmatch       " Show matching brackets.
" Make search case insensitive
set ignorecase      " Do case insensitive matching
set smartcase       " Do smart case matching
set incsearch      " Incremental search
set hlsearch
"set autowrite      " Automatically save before commands like :next and :make
set hidden          " Hide buffers when they are abandoned
set mouse=a        " Enable mouse usage (all modes)

" Source a global configuration file if available
set wrap
set ai
set cin
set lz
set listchars=tab:>.,trail:.,extends:#,nbsp:.
set list
set linebreak
set ruler
set confirm
set t_Co=256
set colorcolumn=130
set number

" tab section
set expandtab        " switch tab into spaces
set tabstop=4        " width of tab
set shiftwidth=4     " for command << and >>
set softtabstop=4    " number of spaces in tab
set smartindent      " set auto indent into new row
set smarttab         " set indent if cursor in begin of row and press tab
set shiftround
set bs=2             " make backspace behave like normal again
set wildmenu
set laststatus=2
"set statusline=%<%f\ [%Y%R%W]%1*%{(&modified)?'\ [+]\ ':''}%*%=%c%V,%l\ %P\ [%n]
       
" Подсвечивать линию текста, на которой находится курсор
set cursorline
" Show whitespace
" MUST be inserted BEFORE the colorscheme command

colorscheme myterm

set pastetoggle=<F2>
set clipboard=unnamedplus
set wildmode=list:full
set enc=utf-8
set ls=2

" Useful settings
set history=700
set undolevels=700

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile

set ttyfast

" vim-airline
let g:airline#extensions#tabline#enabled = 1

" Settings for jedi-vim
let g:jedi#usages_command = "<leader>z"
let g:jedi#popup_on_dot = 1
let g:jedi#popup_select_first = 1
map <Leader>b Oimport ipdb; ipdb.set_trace() # BREAKPOINT<C-c>

" IndentLine plugin
let g:indentLine_char = '|'
let g:indentLine_leadingSpaceChar = '·'
let g:indentLine_leadingSpaceEnabled = 1
