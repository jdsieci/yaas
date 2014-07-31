-record(check, {user :: #user{},
                password :: string()
               }).
-record(delete, {username :: string(),
                 realm :: string()
                }).
-record(add, {user :: #user{},
              props = [] :: yaas:properties()
             }).
-record(update, {user :: #user{},
                 props = [] :: yaas:properties()
                }).