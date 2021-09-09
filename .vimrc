" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" LEADER
nnoremap <SPACE> <Nop>
let mapleader=" "
" LEADER END

syntax on

if &shell =~# 'fish$'
    set shell=bash
endif

set nocompatible
set background=dark
set termguicolors

set showcmd          " Show (partial) command in status line.
set showmode         " Show mode
set showmatch        " Show matching brackets.
set matchpairs+=<:>  " Highlight html brackets
set matchtime=3      " Always highlight brackets
set ignorecase       " Do case insensitive matching
set smartcase        " Do smart case matching
set incsearch        " Incremental search
set hlsearch         " highlighting search word
set infercase        " Register help
set hidden           " Hide buffers when they are abandoned
set novisualbell     " turn off bell
set wrap
set autoindent       " Copy indent from current line when starting a new line
set cindent          " auto indent for c programming files
set lazyredraw       " screen will not be redrawn while executing macros, registers and other commands
set listchars=eol:$,tab:>-,trail:~,extends:#,precedes:<,nbsp:%
set list             " Show tabs as CTRL-I is displayed
set linebreak
set ruler            " position of cursor
set confirm
set title            " '-' file can't change, '+' modified file, '=' readonly
set autoread         " check if file not changed by another editor
set smartindent      " set auto indent into new row
set smarttab         " set indent if cursor in begin of row and press tab
set shiftround       " Round indent to multiple of 'shiftwidth'.  Applies to > and <
set bs=2             " make backspace behave like normal again
set laststatus=2
set tabpagemax=30    " max opened tabs

set statusline=
set statusline+=%#DiffAdd#%{(mode()=='n')?'\ \ NORMAL\ ':''}
set statusline+=%#DiffChange#%{(mode()=='i')?'\ \ INSERT\ ':''}
set statusline+=%#DiffDelete#%{(mode()=='r')?'\ \ RPLACE\ ':''}
set statusline+=%#Cursor#%{(mode()=='v')?'\ \ VISUAL\ ':''}
set statusline+=\ %n\           " buffer number
set statusline+=%#Visual#       " colour
set statusline+=%{&paste?'\ PASTE\ ':''}
set statusline+=%{&spell?'\ SPELL\ ':''}
set statusline+=%#CursorIM#     " colour
set statusline+=%R              " readonly flag
set statusline+=%M              " modified [+] flag
set statusline+=%#Cursor#       " colour
set statusline+=%#CursorLine#   " colour
set statusline+=\ %t\           " short file name
set statusline+=%=              " right align
set statusline+=%#CursorLine#   " colour
set statusline+=\ %Y\           " file type
set statusline+=%#CursorIM#     " colour
set statusline+=\ %3l:%-2c\     " line + column
set statusline+=%#Cursor#       " colour
set statusline+=\ %3p%%\        " percentage

set cursorline
set clipboard=unnamedplus

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile

set pastetoggle=<F3>
set wildmenu                    " command line completion
set wildmode=list:full
set encoding=utf-8
set fileformat=unix             " forman file ending

set history=700
set undolevels=700

" set Ignore file
set wildignore+=*/tmp/*,*.so,*.swp,*.pyc,**/bower_components/**,**/node_modules/**,**/.git/**,**/vendor/**

" Gruvbox settings
let g:gruvbox_italic=1
colorscheme gruvbox

autocmd InsertEnter * highlight CursorLine guibg=#0a0a0f
autocmd InsertLeave * highlight CursorLine guibg=bg

autocmd BufNewFile,BufFilePre,BufRead,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufFilePre,BufRead,BufReadPost vifm* set filetype=vifm
" jump to the last position when reopening a file
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" change coursor shape in different modes
let &t_SI = "\<Esc>[6 q"
let &t_EI = "\<Esc>[2 q"

" MAPS
vnoremap <C-c> "+y            " copy selected text Ctrl+c
nnoremap ; :
vnoremap ; :

" previous buffer
map <F5> :bp<CR>
vmap <F5> <Esc>:bp<CR>i
imap <F5> <Esc>:bp<CR>i
" next buffer
map <F6> :bn<CR>
vmap <F6> <Esc>:bn<CR>i
imap <F6> <Esc>:bn<CR>i

" JSON PRETTIFY
nnoremap <leader>jp :%!python -m json.tool<cr>

nmap ] }
nmap [ {

inoremap jk <Esc>
inoremap <C-a> <C-o>0
inoremap <C-e> <C-o>$
noremap <leader>s :ls -la<CR>
nnoremap <leader>/ :Grep<Space>
nnoremap <leader>k :bd<CR>
nnoremap <leader>d yyp<CR>
nnoremap <leader>l yy<CR>
nnoremap <Leader>b :ls<CR>:b<Space>

nnoremap ;Q :quit<CR>
nnoremap <C-x> :quit<CR>
nnoremap <leader>q :quit
" nnoremap <NUL> :%s/\s\+$//e

" The Silver Searcher
if executable('rg')
  " Use ag over grep
  set grepprg=rg\ --color\ auto\ --vimgrep
  set grepformat=%f:%l:%c:%m
  nnoremap <leader>/ :grep <cword><CR>:cwindow<CR>
endif

" Comment section
let s:comment_map = {
    \   "c": '// ',
    \   "cpp": '// ',
    \   "go": '// ',
    \   "java": '// ',
    \   "javascript": '// ',
    \   "php": '// ',
    \   "python": '# ',
    \   "ruby": '# ',
    \   "vim": '" ',
    \   "bash": '# ',
    \   "sh": '# ',
    \   "scala": '// ',
    \   "el": '; ',
    \ }

function! CommentToggle()
  if has_key(s:comment_map, &filetype)
    let comment_leader = s:comment_map[&filetype]
      if getline('.') =~ "^" . comment_leader
        " Uncomment the line
        execute "silent s/^" . comment_leader . "//"
      else
        " Comment the line
        execute "silent s/^/" . comment_leader . "/"
      endif
  else
    echo "No comment leader found for filetype"
  end
endfunction

noremap <M-;> :call CommentToggle()<cr>

" tab section
" virtual tabstops using spaces
let my_tab=4
execute "set shiftwidth=".my_tab
execute "set softtabstop=".my_tab
execute "set tabstop=".my_tab
set expandtab        " switch tab into spaces

" allow toggling between local and default mode
function! TabToggle()
  if &expandtab
    set noexpandtab
  else
    set expandtab
  endif
endfunction

" relative numbers
set number

" augroup numbertoggle
"   autocmd!
"   autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
"   autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
" augroup END
" nnoremap <C-n> :call NumberToggle()<cr>

" file browser section
let g:netrw_liststyle     = 3
let g:netrw_browse_split  = 3
let g:netrw_altv          = 1
let g:netrw_fastbrowse    = 2
let g:netrw_keepdir       = 1
let g:netrw_retmap        = 1
let g:netrw_silent        = 1
let g:netrw_special_syntax= 1

map <F7> :Texplore<CR>

if &diff
  map ] ]c
  map [ [c
endif

" wayland section
if executable('wl-copy')
   xnoremap "+y y:call system("wl-copy", @")<cr>
   nnoremap "+p :let @"=substitute(system("wl-paste --no-newline"), '<C-v><C-m>', '', 'g')<cr>p
   nnoremap "*p :let @"=substitute(system("wl-paste --no-newline --primary"), '<C-v><C-m>', '', 'g')<cr>p
endif
