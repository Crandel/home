" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

set nocompatible

syntax on

set background=dark

if &shell =~# 'fish$'
    set shell=bash
endif
" Uncomment the following to have Vim jump to the last position when
" reopening a file
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showcmd         " Show (partial) command in status line.
set showmatch       " Show matching brackets.
" Make search case insensitive
set ignorecase      " Do case insensitive matching
set smartcase       " Do smart case matching
set incsearch       " Incremental search
set hlsearch        " highlighting search word
set infercase       " Register help
set hidden          " Hide buffers when they are abandoned
" turn off bell
set novisualbell
" Source a global configuration file if available
set wrap
set ai
set cin
set lz
set listchars=eol:$,tab:>-,trail:~,extends:#,precedes:<,nbsp:%
set list
set linebreak
set ruler
set confirm
set ttyfast
set title
set autoread         " check if file not changed by another editor
set smartindent      " set auto indent into new row
set smarttab         " set indent if cursor in begin of row and press tab
set shiftround
set bs=2             " make backspace behave like normal again
set wildmenu
set laststatus=2
set tabpagemax=30    " max opened tabs
set statusline=%<%f\ [%Y%R%W]%1*%{(&modified)?'\ [+]\ ':''}%*%=%c%V,%l\ %P\ [%n]

set cursorline

set clipboard=unnamedplus

vnoremap <C-c> "+y
set pastetoggle=<F3>
set wildmode=list:full
set ls=2
set fileformat=unix    " forman file ending

" previous buffer
map <F5> :bp<CR>
vmap <F5> <Esc>:bp<CR>i
imap <F5> <Esc>:bp<CR>i
" next buffer
map <F6> :bn<CR>
vmap <F6> <Esc>:bn<CR>i
imap <F6> <Esc>:bn<CR>i
" Useful settings
set history=700
set undolevels=700
" swap the current word with the next, without changing cursor position
nnoremap <silent> gw "_yiw:s/\(\%#\w\+\)\(\W\+\)\(\w\+\)/\3\2\1/<CR><c-o><c-l>
" JSON PRETTIFY
nnoremap <leader>jp :%!python -m json.tool<cr>

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile
" set Ignore file
set wildignore+=*/tmp/*,*.so,*.swp,*.pyc,**/bower_components/**,**/node_modules/**,**/.git/**,**/vendor/**

noremap <space>s :ls<cr>
nnoremap <space>/ :Grep<Space>

au BufNewFile,BufFilePre,BufRead *.md set filetype=markdown

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --hidden\ --vimgrep
  set grepformat=%f:%l:%c:%m
  nnoremap <leader>gg :grep <cword><CR>:cwindow<CR>
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
    \   "el": '; ',
    \ }

function! ToggleComment()
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

execute "set <M-;>=\e;"
noremap <M-;> :call ToggleComment()<cr>
map <F9> Oimport pdb; pdb.set_trace() # BREAKPOINT

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
nmap <F10> mz:execute TabToggle()<CR>'z

"relative numbers
set nu

" file browser section
let g:netrw_liststyle     = 3
let g:netrw_browse_split  = 3
let g:netrw_altv          = 1
let g:netrw_fastbrowse    = 2
let g:netrw_keepdir       = 1
let g:netrw_retmap        = 1
let g:netrw_silent        = 1
let g:netrw_special_syntax= 1
" augroup ProjectDrawer
"   autocmd!
"   autocmd VimEnter * :Vexplore
" augroup END
map <F7> :Texplore<CR>

nnoremap <NUL> :%s/\s\+$//e

nnoremap <C-n> :call NumberToggle()<cr>

" change coursor shape in different modes
let &t_SI = "\<Esc>[6 q"
let &t_EI = "\<Esc>[2 q"