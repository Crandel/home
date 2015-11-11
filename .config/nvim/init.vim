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

" https://github.com/airblade/vim-gitgutter
Plugin 'airblade/vim-gitgutter'

" https://github.com/bling/vim-airline
Plugin 'bling/vim-airline'

" https://github.com/cwood/vim-django
" Plugin 'cwood/vim-django'

" https://github.com/dag/vim-fish
Plugin 'dag/vim-fish'

" https://github.com/davidhalter/jedi-vim
Plugin 'davidhalter/jedi-vim'

" https://github.com/dkprice/vim-easygrep
Plugin 'dkprice/vim-easygrep'

" https://github.com/easymotion/vim-easymotion
Plugin 'easymotion/vim-easymotion'

" https://github.com/fatih/vim-go
Plugin 'fatih/vim-go'

" https://github.com/jiangmiao/auto-pairs
Plugin 'jiangmiao/auto-pairs'

" https://github.com/kien/ctrlp.vim
Plugin 'kien/ctrlp.vim'

" https://github.com/klen/python-mode
Plugin 'klen/python-mode'

" https://github.com/majutsushi/tagbar
Plugin 'majutsushi/tagbar'

" https://github.com/mattn/emmet-vim
Plugin 'mattn/emmet-vim'

" https://github.com/othree/html5.vim
Plugin 'othree/html5.vim'

" https://github.com/scrooloose/nerdtree
Plugin 'scrooloose/nerdtree'

" https://github.com/scrooloose/syntastic
Plugin 'scrooloose/syntastic'

" https://github.com/SirVer/ultisnips
Plugin 'SirVer/ultisnips'

" https://github.com/tpope/vim-fugitive
Plugin 'tpope/vim-fugitive'

" https://github.com/tpope/vim-surround
Plugin 'tpope/vim-surround'

" https://github.com/Valloric/YouCompleteMe
Plugin 'Valloric/YouCompleteMe', { 'do': 'python2 ./install.py --gocode-completer' }

" https://github.com/Xuyuanp/nerdtree-git-plugin
Plugin 'Xuyuanp/nerdtree-git-plugin'

" https://github.com/Yggdroot/indentLine
Plugin 'Yggdroot/indentLine'

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

"vim-go
au FileType go nmap <Leader>d <Plug>(go-def)
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_command = "goimports"

"" YouCompleteMe settings
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

" Git
nmap <C-g> :Gblame<CR>
nmap <F12> :Gdiff<CR>
" javascript
let g:javascript_enable_domhtmlcss = 1

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

noremap <C-n> :call NumberToggle()<cr>
