" Specify a directory for plugins
"
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
Plug 'vim-scripts/Zenburn'
Plug 'lifepillar/vim-solarized8'
" Make sure you use single quotes
" plugin on GitHub repo
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'mileszs/ack.vim'
Plug 'kien/ctrlp.vim'
" Plug 'spwhitt/vim-nix'
" Plug 'tyru/open-browser.vim'
" Plug 'ntpeters/vim-better-whitespace'
" Plug 'lervag/vimtex'
" Plug 'ervandew/supertab'
" Plug 'godlygeek/tabular'
" Plug 'easymotion/vim-easymotion'
" Plug 'jaspervdj/stylish-haskell'
Plug 'christoomey/vim-tmux-navigator'
" Plug 'sjl/gundo.vim'
" Plug 'tpope/vim-unimpaired'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/syntastic'
Plug 'derekwyatt/vim-scala'
"Plug 'neoclide/coc.nvim', {'branch': 'release'}

"Plug 'keith/swift.vim'
Plug 'valloric/youcompleteme'
"Plug 'Quramy/tsuquyomi'
Plug 'racer-rust/vim-racer'
Plug 'aiya000/vim-ghcid-quickfix'
Plug 'tpope/vim-cucumber'
Plug 'prabirshrestha/async.vim'
"Plug 'prabirshrestha/vim-lsp'
" Initialize plugin system
call plug#end()

" NerdTree
map <C-n> :NERDTreeToggle<CR>

" :Spelling
"set spell spelllang=en_us
" DAML
if executable('daml')
   au User lsp_setup call lsp#register_server({
      \ 'name': 'daml-ide',
      \ 'cmd': {server_info->['daml', 'damlc' , 'ide']},
      \ 'initialization_options': {},
      \ 'whitelist': ['daml'],
      \ })
endif
if executable('metals-vim')
   au User lsp_setup call lsp#register_server({
      \ 'name': 'metals',
      \ 'cmd': {server_info->['metals-vim']},
      \ 'initialization_options': { 'rootPatterns': 'build.sbt' },
      \ 'whitelist': [ 'scala', 'sbt' ],
      \ })
endif

let g:ycm_language_server = 
  \ [ 
  \   {
  \     'name': 'daml',
  \     'cmdline': [ '/home/gyorgy/.daml/bin/daml', 'damlc', 'ide' ],
  \     'filetypes': [ 'daml' ]
  \   },
  \   {
  \     'name': 'rust',
  \     'cmdline': [ 'ra_lsp_server' ],
  \     'filetypes': [ 'rust' ],
  \     'project_root_files': [ 'Cargo.toml' ]
  \   }
  \ ]

au BufRead,BufNewFile *.daml set filetype=daml
autocmd BufNewFile,BufRead *.daml set syntax=daml

"let g:lsp_log_verbose = 1
"let g:lsp_log_file = expand('~/vim-lsp.log')
"let g:lsp_auto_enable = 1
"let g:lsp_diagnostics_enabled = 1         " disable diagnostics support

" Racer
" set hidden
" let g:racer_cmd = "/home/gyorgy/.cargo/bin/racer"
" let g:racer_experimental_completer = 1
" Syntactics
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
"let g:syntastic_swift_checkers = ['swiftpm']
"let g:syntastic_scala_checkers = ['scalac']
" set background=dark
"colorscheme solarized8
"let g:airline_theme='solarized' 

" Configuration for vim-scala
au BufRead,BufNewFile *.sbt set filetype=scala

autocmd FileType json syntax match Comment +\/\/.\+$+

let g:ghcid_quickfix_showing = 'quickfix_on_error'

let g:ackprg = 'ag --nogroup --nocolor --column'
cnoreabbrev Ack Ack!

" COC
" if hidden is not set, TextEdit might fail.
set hidden
"
" " Some servers have issues with backup files, see #649
set nobackup
set nowritebackup
"
" " Better display for messages
set cmdheight=2
"
" " You will have bad experience for diagnostic messages when it's default
" 4000.
set updatetime=300
"
" " don't give |ins-completion-menu| messages.
set shortmess+=c
"
" " always show signcolumns
set signcolumn=yes
"
" " Use tab for trigger completion with characters ahead and navigate.
" " Use command ':verbose imap <tab>' to make sure tab is not mapped by other
" plugin.
"inoremap <silent><expr> <TAB>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ coc#refresh()

"inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
"
"function! s:check_back_space() abort
"  let col = col('.') - 1
"  return !col || getline('.')[col - 1]  =~# '\s'
"endfunction
"
"                       " Use <c-space> to trigger completion.
"inoremap <silent><expr> <c-space> coc#refresh()
"
"                       " Use <cr> to confirm completion, `<C-g>u` means break
"                       undo chain at current position.
"                       " Coc only does snippet and additional edit on
"                       confirm.
"inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
"                       " Or use `complete_info` if your vim support it, like:
"                       " inoremap <expr> <cr> complete_info()["selected"] !=
"                       "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
"
"                       " Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
"
"                       " Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
"
"                       " Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>
"
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
"
"                                     " Highlight symbol under cursor on
"                                     CursorHold
" autocmd CursorHold * silent call CocActionAsync('highlight')
"
"                                     " Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)
"
"                                     " Remap for format selected region
xmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)
" Using CocList
" " Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" " Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" " Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" " Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" " Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" " Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" " Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" " Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

set relativenumber
" emacs-y bindings for editing
inoremap <c-e> <esc>$i
inoremap <c-a> <esc>^i
inoremap <c-f> <esc>Wi
inoremap <c-b> <esc>Bi
inoremap <c-d> <esc>xi
inoremap <c-k> <esc>d$i
