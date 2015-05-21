execute pathogen#infect()
" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %
set nocompatible
syntax on
filetype indent on
" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

" Uncomment the following to have Vim jump to the last position when
" reopening a file
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

if &term =~ "terminator"
    let &t_SI = "\<Esc>]12;purple\x7"
    let &t_SR = "\<Esc>]12;red\x7"
    let &t_EI = "\<Esc>]12;blue\x7"
endif

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
set incsearch       " Incremental search
set hlsearch        " highlighting search word
set infercase       " подсказка по регистру
"set autowrite      " Automatically save before commands like :next and :make
set hidden          " Hide buffers when they are abandoned
set mouse=a        " Enable mouse usage (all modes)
" отключаем пищалку и мигание
set novisualbell
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
set title
set autoread         " check if file not changed by another editor

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
set tabpagemax=30    " max opened tabs
"set statusline=%<%f\ [%Y%R%W]%1*%{(&modified)?'\ [+]\ ':''}%*%=%c%V,%l\ %P\ [%n]
       
" Подсвечивать линию текста, на которой находится курсор
set cursorline
" Show whitespace
" MUST be inserted BEFORE the colorscheme command

colorscheme myterm

set pastetoggle=<F3>
set clipboard=unnamedplus
set wildmode=list:full
set enc=utf-8
set ls=2
set fileformat=unix    " forman file ending
" Сохранить файл по <F2>
nmap <F2> :w<CR>
imap <F2> <Esc>:w<CR>
vmap <F2> <Esc>:w<CR>
" Выйти из редактора по <F10>
nmap <F10> :q<CR>
imap <F10> <Esc>:q<CR>
vmap <F10> <Esc>:q<CR>
" Просмотр списка буферов по <F4>
nmap <F4> <Esc>:buffers<CR>
vmap <F4> <Esc>:buffers<CR>
imap <F4> <Esc><Esc>:buffers<CR>
" предыдущий буфер
map <F5> :bp<CR>
vmap <F5> <Esc>:bp<CR>i
imap <F5> <Esc>:bp<CR>i
" следующий буфер
map <F6> :bn<CR>
vmap <F6> <Esc>:bn<CR>i
imap <F6> <Esc>:bn<CR>i
" Useful settings
set history=700
set undolevels=700

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile
" set Ignore file
set wildignore+=*/tmp/*,*.so,*.swp,*.pyc
set ttyfast

" vim-airline
let g:airline#extensions#tabline#enabled = 1

" Settings for jedi-vim
let g:jedi#popup_on_dot = 0 
let g:jedi#popup_select_first = 0
let g:jedi#goto_assignments_command = "<leader>g"
let g:jedi#goto_definitions_command = "<leader>d"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<leader>n"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>r"
map <Leader>b Oimport ipdb; ipdb.set_trace() # BREAKPOINT<C-c>

"" YouCompleteMe settings
let g:ycm_collect_identifiers_from_tags_files = 1 " Let YCM read tags from Ctags file
let g:ycm_use_ultisnips_completer = 1 " Default 1, just ensure
let g:ycm_seed_identifiers_with_syntax = 1 " Completion for programming language's keyword
let g:ycm_complete_in_comments = 1 " Completion in comments
let g:ycm_complete_in_strings = 1 " Completion in string

" IndentLine plugin
let g:indentLine_char = '|'
let g:indentLine_leadingSpaceChar = '·'
let g:indentLine_leadingSpaceEnabled = 1

" ultisnips
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'
" django
let g:django_activate_virtualenv = 1 "Try to activate the associated virtualenv
let g:django_activate_nerdtree = 1 "Try to open nerdtree at the project root.
let g:django_projects = '~/work/projects' "Sets all projects under project
let g:python_highlight_all = 1
" Html5
let g:html_indent_inctags = "html,body,head,tbody"
"Disable event-handler attributes support:

let g:html5_event_handler_attributes_complete = 0
"Disable RDFa attributes support:

let g:html5_rdfa_attributes_complete = 0
"Disable microdata attributes support:

let g:html5_microdata_attributes_complete = 0
"Disable WAI-ARIA attribute support:

let g:html5_aria_attributes_complete = 0
