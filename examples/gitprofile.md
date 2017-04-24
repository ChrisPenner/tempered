#!/Users/chris/.local/bin/tempered
Chris's github account has {{
    account_info=`curl https://api.github.com/users/chrispenner`
    followers=`json followers << $account_info`
    echo $account_info | json followers  }} followers!

