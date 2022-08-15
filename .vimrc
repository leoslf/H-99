" Project-level .vimrc
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set nosmarttab

function! RunAllTests(runner, args, ...) abort
  let test_command = g:test#{a:runner}#test_command
  let args = [test_command] + a:args
  if type(get(g:, 'test#strategy')) == type({})
    let strategy = get(g:test#strategy, a:type)
    call test#execute(a:runner, args, strategy)
  else
    call test#execute(a:runner, args)
  endif
endfunction

command! -nargs=* -bar TestAll  call RunAllTests('haskell#stacktest', split(<q-args>))

nmap <silent> <leader>t :TestAll<CR>
