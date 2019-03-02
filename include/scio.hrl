
% Model Records

-record(user, {
    id          ::integer(),
    uuid        ::bitstring(),
    username    ::bitstring(),
    email       ::bitstring(),
    password    ::bitstring()
}).


-record(session, {
    session_id   :: bitstring(),
    user_id    = undefined  :: integer()   | 'undefined',
    created_at = undefined  :: integer()   | 'undefined'
}).


-record(shortcut, {
    id              ::integer(),
    url             ::bitstring(),
    title           ::bitstring(),
    description     ::bitstring(),
    screenshot_id   ::bitstring(),
    user_id         ::integer(),
    created_at      ::bitstring(),
    updated_at      ::bitstring()
}).
