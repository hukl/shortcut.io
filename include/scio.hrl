
% Model Records

-record(user, {
    id          ::bitstring(),
    uuid        ::bitstring(),
    username    ::bitstring(),
    email       ::bitstring(),
    password    ::bitstring()
}).


-record(session, {
    session_id   :: bitstring(),
    user_id    = undefined  :: bitstring() | 'undefined',
    created_at = undefined  :: integer()   | 'undefined'
}).


-record(shortcut, {
    id              ::bitstring(),
    url             ::bitstring(),
    title           ::bitstring(),
    description     ::bitstring(),
    screenshot_id   ::bitstring(),
    user_id         ::bitstring(),
    created_at      ::bitstring(),
    updated_at      ::bitstring()
}).
