execute pathogen#infect()
" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %
set nocompatible
syntax on
colorscheme myterm

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

if &shell =~# 'fish$'
    set shell=bash
endif
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
set number
set title
set autoread         " check if file not changed by another editor
set smartindent      " set auto indent into new row
"set smarttab         " set indent if cursor in begin of row and press tab
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

set clipboard=unnamedplus
vnoremap <C-c> "+y
set pastetoggle=<F3>
set wildmode=list:full
set enc=utf-8
set ls=2
set fileformat=unix    " forman file ending
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

" NerdTree start section
let g:NERDTreeShowHidden=1
map <F7> :NERDTreeToggle<CR>
" NERDTress File highlighting
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
 exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
 exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ini', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('styl', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', '#151515')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')
call NERDTreeHighlightFile('py', 'Magenta', 'none', '#ff00ff', '#151515')
" NerdTree start if empty vim
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" close vim if the only window left open is a NERDTree
"autocmd bufenter * if len(filter(range(1, bufnr('$')), '! empty(bufname(v:val)) && buflisted(v:val)')) == 1 && (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
let NERDTreeIgnore=['\.pyc$', '\.pyo$']
" End NERDTree section

" vim-airline
let g:airline#extensions#tabline#enabled = 1


" python-mode
let g:pymode_options = 0
let g:pymode_indent = 0
let g:pymode_lint_ignore = "W191"
let g:pymode_rope_goto_definition_bind = "<Leader>d"
let g:pymode_options_max_line_length = 130
let g:pymode_lint_on_write = 0
let g:pymode_folding = 0
let g:pymode_breakpoint = 0
let g:pymode_rope_completion = 0
let g:pymode_rope_complete_on_dot = 0
let g:pymode_rope_project_root = "/opt/work/backup/"
let g:pymode_rope_ropefolder='/opt/work/backup'
let g:pymode_rope_goto_definition_cmd = 'e'
map <Leader>b Oimport pdb; pdb.set_trace() # BREAKPOINT

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
let g:django_projects = $MY_PROJECTS_ROOT "Sets all projects under project
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

" Emmet
let g:user_emmet_mode='a'
let g:user_emmet_leader_key='<leader>'

" TagBar
nmap <F10> :TagbarToggle<CR>

" Git
nmap <C-g> :Gblame<CR>
nmap <F12> :Gdiff<CR>

" tab section
" virtual tabstops using spaces
let my_tab=4
execute "set shiftwidth=".my_tab
execute "set softtabstop=".my_tab
execute "set tabstop=".my_tab
set noexpandtab        " switch tab into spaces
" allow toggling between local and default mode
function! TabToggle()
  if &expandtab
    set noexpandtab
  else
    set expandtab
  endif
endfunction
nmap <F9> mz:execute TabToggle()<CR>'z
filetype plugin indent on
