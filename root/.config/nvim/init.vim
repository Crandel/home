" Automatic reloading of .vimrc
autocmd! bufwritepost init.vim source %
let g:python_host_prog='/usr/bin/python2'
filetype off                  " required
" set the runtime path to include Vundle and initialize
set rtp+=~/.config/nvim/bundle/Vundle.vim
call vundle#begin("~/.config/nvim/bundle")
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" https://github.com/bling/vim-airline
Plugin 'bling/vim-airline'

" https://github.com/dag/vim-fish
Plugin 'dag/vim-fish'

" https://github.com/dkprice/vim-easygrep
Plugin 'dkprice/vim-easygrep'

" https://github.com/easymotion/vim-easymotion
Plugin 'easymotion/vim-easymotion'

" https://github.com/kien/ctrlp.vim
Plugin 'kien/ctrlp.vim'

" https://github.com/Matt-Deacalion/vim-systemd-syntax
Plugin 'Matt-Deacalion/vim-systemd-syntax'

" https://github.com/ekalinin/Dockerfile.vim
Plugin 'ekalinin/Dockerfile.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

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
set infercase       " подсказка по регистру
"set autowrite      " Automatically save before commands like :next and :make
set hidden          " Hide buffers when they are abandoned
set regexpengine=1
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
set smartindent      " set auto indent into new row
set shiftround
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

" vim-airline
let g:airline#extensions#tabline#enabled = 1
" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'

"Easy-motion
let g:EasyMotion_do_mapping = 0 " Disable default mappings
" Bi-directional find motion
" Jump to anywhere you want with minimal keystrokes, with just one key
" binding.
" `s{char}{label}`
"nmap s <Plug>(easymotion-s)
" or
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap s <Plug>(easymotion-s2)
"
"" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1
map <Leader>w <Plug>(easymotion-w)
" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
" Search
map  / <Plug>(easymotion-sn)

" These `n` & `N` mappings are options. You do not have to map `n` & `N` to
" EasyMotion.
" Without these mappings, `n` & `N` works fine. (These mappings just provide
" different highlight method and have some other features )
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)"

" Easy-grep
let g:EasyGrepRecursive = 1
let g:EasyGrepCommand = 1
let g:EasyGrepFilesToExclude=".svn,.git"

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
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

map <Leader>/ :call ToggleComment()<cr>

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
nmap <F9> mz:execute TabToggle()<CR>'z

"relative numbers
function! NumberToggle()
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

set rnu
function ToggleNumbersOn()
    set rnu!
    set nu
endfunction
function ToggleRelativeOn()
    set nu!
    set rnu
endfunction

autocmd FocusLost * call ToggleNumbersOn()
autocmd FocusGained * call ToggleRelativeOn()
autocmd InsertEnter * call ToggleNumbersOn()
autocmd InsertLeave * call ToggleRelativeOn()

nnoremap <C-n> :call NumberToggle()<cr>
