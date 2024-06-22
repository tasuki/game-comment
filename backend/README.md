# How to backend

Create user:

    curl -v -XPOST --data '{"username": "vita", "password": "pass"}' http://localhost:6483/users

Log in (TODO: rid of the quotes?):

    JWT=`curl -v -XPOST --data '{"username": "vita", "password": "pass"}' http://localhost:6483/sessions | jq .authToken`

Post a comment:

    curl -v -XPOST --header "Authorization: Bearer $JWT" --data '{"comment": "this is my first comment"}' http://localhost:6483/games/lg/2435451/comments
