set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
syntax on

" wayland clipboard fix (requires wl-clipboard)
if $XDG_SESSION_TYPE == "wayland"
    xnoremap "+y y:call system("wl-copy", @")<CR>
    nnoremap "+p :let @"=substitute(system("wl-paste --no-newline"), '<C-v><C-m>', '', 'g')<CR>p
    nnoremap "*p :let @"=substitute(system("wl-paste --no-newline --primary"), '<C-v><C-m>', '', 'g')<CR>p
endif

" bash like path tab complete
set wildmode=list:longest

" key maps
" map leader to space
let mapleader = " "

" reload config
nnoremap <Leader>rr :source ~/.vim/vimrc<CR>

nnoremap <Leader>> :ls<CR>:buffer 
" list buffers
nnoremap <Leader>bl :ls<CR>
nnoremap <Leader>bn :bnext<CR>
nnoremap <Leader>bp :bprevious<CR>

" map space to window management key
nnoremap <Leader> <C-w>

nnoremap <Leader>. :edit 
nnoremap <Leader>, :tabedit 

" fugitive
nnoremap <leader>gg :Git<CR>:wincmd r<CR>
nnoremap <leader>gp :Git -c push.default=current push<CR>
nnoremap <leader>gc :Git checkout 
nnoremap <leader>gb :Git checkout -b 

" convert github markdown to html
"autocmd BufWritePost *.md !pandoc -r gfm -w html % -o %.html
nnoremap <leader>mm :!pandoc -r gfm -w html % -o %.html<CR><CR>
nnoremap <leader>mo :!firefox %.html &<CR><CR>

" unmap F1 help, definitely don't hit this by accident a lot
map <F1> <nop>

" netrw customizations
" remove banner
let g:netrw_banner = 0
" tree view
let g:netrw_liststyle = 3

" status line
" make always visible
set laststatus=2
" current git branch (requires fugitive)
set statusline=%{fugitive#head()}
" current file
set statusline+=\ %F
" current line/col
set statusline+=%=\ line:%l\ col:%v

" changes current wording dir to dir of current buffer
set autochdir

set autoindent
