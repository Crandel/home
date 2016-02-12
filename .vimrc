" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %
if has('nvim')
    let g:python_host_prog='/usr/bin/python2'
endif
set nocompatible
filetype off                  " required

" set the runtime path to include neobundle and initialize
set rtp+=~/.vim/bundle/neobundle.vim/

call neobundle#begin(expand('~/.vim/bundle/'))
" alternatively, pass a path where neobundle should install plugins
" call neobundle#begin('~/some/path/here')

" let neobundle manage neobundle, required
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimproc.vim', {'build': {'linux': 'make'}}
" NeoBundle 'cwood/vim-django'
NeoBundleLazy 'Matt-Deacalion/vim-systemd-syntax'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'Valloric/YouCompleteMe', {'build': 'python2 install.py --gocode-completer'}
NeoBundleLazy 'Xuyuanp/nerdtree-git-plugin'
NeoBundleLazy 'xolox/vim-session'
NeoBundleLazy 'xolox/vim-misc'
NeoBundle 'Yggdroot/indentLine'
NeoBundleLazy 'airblade/vim-gitgutter'
NeoBundle 'bling/vim-airline'
NeoBundle 'ctrlpvim/ctrlp.vim'

NeoBundleLazy 'dag/vim-fish'
NeoBundleLazy 'davidhalter/jedi-vim', {'build': 'sudo pip install -U jedi', 'autoload': {'filetypes': ['python']}}
NeoBundle 'dkprice/vim-easygrep'
NeoBundle 'easymotion/vim-easymotion'
NeoBundleLazy 'ekalinin/Dockerfile.vim'
NeoBundleLazy 'fatih/vim-go', {'build': 'go get github.com/alecthomas/gometalinter', 'autoload': {'filetypes': ['go']}}
NeoBundleLazy 'hdima/python-syntax', {'autoload':{'filetypes':['python']}}
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'majutsushi/tagbar'
NeoBundleLazy 'mattn/emmet-vim', {'autoload':{'filetypes': ['html', 'xhttml', 'css']}}
NeoBundleLazy 'othree/html5.vim', {'autoload':{'filetypes': ['html', 'xhttml', 'css']}}
"NeoBundle 'racer-rust/vim-racer', {'build': 'cargo install --git https://github.com/phildawes/racer.git'}
"NeoBundle 'rust-lang/rust.vim', {'build': 'cargo install --git https://github.com/rust-lang-nursery/rustfmt'}
NeoBundleLazy 'scrooloose/nerdtree'
NeoBundleLazy 'scrooloose/syntastic'
NeoBundleLazy 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-surround'
NeoBundleLazy 'jelera/vim-javascript-syntax', {'autoload':{'filetypes':['javascript, html']}}

" All of your NeoBundles must be added before the following line
call neobundle#end()         " required

filetype plugin indent on    " required

syntax on
colorscheme behelit

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

if &shell =~# 'fish$'
    set shell=bash
endif
" Uncomment the following to have Vim jump to the last position when
" reopening a file
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

let &t_SI = "\<Esc>]12;purple\x7"
let &t_SR = "\<Esc>]12;red\x7"
let &t_EI = "\<Esc>]12;blue\x7"

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
set regexpengine=1
" отключаем пищалку и мигание
set novisualbell
" Source a global configuration file if available
set wrap
set ai
set cin
set lz
set listchars=eol:¬,tab:>-,trail:~,extends:#,precedes:<
set colorcolumn=130
set list
set linebreak
set ruler
set confirm
set t_Co=256
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
" swap the current word with the next, without changing cursor position
nnoremap <silent> gw "_yiw:s/\(\%#\w\+\)\(\W\+\)\(\w\+\)/\3\2\1/<CR><c-o><c-l>

" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile
" set Ignore file
set wildignore+=*/tmp/*,*.so,*.swp,*.pyc,**/bower_components/**,**/node_modules/**,**/.git/**,**/vendor/**
" Neobundle
let g:neobundle#log_filename = $HOME.'/.vim/tmp/neobundle.log'

noremap <space>s :ls<cr>
nnoremap <space>/ :Grep<Space>

" Python Syntax
let g:python_highlight_all = 1

" vim session
let g:session_autoload = 0
let g:session_autosave = 0
let g:session_command_aliases = 1

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
let g:airline_powerline_fonts = 1

" Settings for jedi-vim
let g:jedi#completions_enabled = 0
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
let g:jedi#show_call_signatures = "2"
let g:jedi#goto_assignments_command = "<leader>g"
let g:jedi#goto_definitions_command = "<leader>d"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "<leader>n"
let g:jedi#rename_command = "<leader>r"
let g:jedi#use_tag_stack = 0
au FileType python setlocal completeopt-=preview
map <Leader>b Oimport ipdb; ipdb.set_trace() # BREAKPOINT

" Syntastic
map <F4> :SyntasticCheck<CR>
let g:syntastic_python_checkers = ['flake8']
let g:syntastic_go_checkers = ['gometalinter']
let g:syntastic_python_flake8_args='--ignore="W191" --max-line-length=130'
let g:syntastic_aggregate_errors = 1
let g:syntastic_always_populate_loc_list = 1
nmap <Leader>l :lne<CR>

" vim-go
au FileType go nmap <Leader>d <Plug>(go-def)
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"

" rust.vim
"let g:rustfmt_autosave = 1

" vim-racer
"set hidden
"let g:racer_cmd = $RUST.'/bin'
"let $RUST_SRC_PATH = $RUST.'/src/rust'

" YouCompleteMe settings
let g:ycm_collect_identifiers_from_tags_files = 1 " Let YCM read tags from Ctags file
let g:ycm_use_ultisnips_completer = 1 " Default 1, just ensure
let g:ycm_seed_identifiers_with_syntax = 1 " Completion for programming language's keyword
let g:ycm_complete_in_comments = 1 " Completion in comments
let g:ycm_complete_in_strings = 1 " Completion in string
let g:ycm_complete_in_comments = 1 
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_autoclose_preview_window_after_completion = 1

" IndentLine plugin
let g:indentLine_char = '|'
let g:indentLine_leadingSpaceChar = '·'
let g:indentLine_color_term = 239
let g:indentLine_leadingSpaceEnabled = 1

" ultisnips
let g:UltiSnipsExpandTrigger="<c-a>"
let g:UltiSnipsJumpForwardTrigger="<c-a>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

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
let g:EasyGrepFilesToExclude=".svn,.git,.idea"

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --hidden\ --vimgrep
  set grepformat=%f:%l:%c:%m
  nnoremap <leader>gg :grep <cword><CR>:cwindow<CR>

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" django
"let g:django_activate_virtualenv = 1 "Try to activate the associated virtualenv
"let g:django_activate_nerdtree = 1 "Try to open nerdtree at the project root.
"let g:django_projects = $MY_PROJECTS_ROOT "Sets all projects under project
"let g:python_highlight_all = 1

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

" Git Fugitive
nmap <C-g> :Gblame<CR>
nmap <F12> :Gdiff<CR>

" javascript
let g:javascript_enable_domhtmlcss = 1

"Git Gutter
"nmap <C-.> <Plug>GitGutterNextHunk
"nmap <C-,> <Plug>GitGutterPrevHunk

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
set nu
set rnu
function! NumberToggle()
  if(&relativenumber == 1)
    set rnu!
  else
    set rnu
  endif
endfunc

function! ToggleNumbersOn()
    set rnu!
endfunction
function! ToggleRelativeOn()
    set rnu
endfunction

autocmd FocusLost * call ToggleNumbersOn()
autocmd FocusGained * call ToggleRelativeOn()
autocmd InsertEnter * call ToggleNumbersOn()
autocmd InsertLeave * call ToggleRelativeOn()

nnoremap <NUL> :%s/\s\+$//e

nnoremap <C-n> :call NumberToggle()<cr>
