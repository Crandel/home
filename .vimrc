" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %
set nocompatible
syntax on
colorscheme myterm
set rtp=~/.vim/bundle/vim_lib
call vim_lib#sys#Autoload#init('~/.vim', 'bundle')

Plugin 'vim_lib'
Plugin 'ctrlp'
Plugin 'auto-pairs'
Plugin 'EasyGrep'
Plugin 'emmet-vim'
Plugin 'html5.vim'
Plugin 'indentLine'
Plugin 'jedi-vim'
Plugin 'nerdtree'
Plugin 'syntastic'
Plugin 'tagbar'
Plugin 'ultisnips'
Plugin 'vim-airline'
Plugin 'vim-autopep8'
Plugin 'vim_plugmanager'
Plugin 'YouCompleteMe'
Plugin 'vim-snippets'
Plugin 'vim-fish'
Plugin 'vim-django'
Plugin 'vim-go'
Plugin 'vim-surround'
Plugin 'vim_prj'
Plugin 'vim_git'
Plugin 'vim_grep'
Plugin 'vim_write'
Plugin 'vim_winmanager'
" Plugin 'vim_start'

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
set relativenumber
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


set pastetoggle=<F3>
set clipboard=unnamed
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
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
let NERDTreeIgnore=['\.pyc$', '\.pyo$']
" End NERDTree section

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

" Autopep8
let g:autopep8_max_line_length=130

" Emmet
let g:user_emmet_mode='a'
let g:user_emmet_leader_key='<leader>'
