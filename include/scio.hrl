
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
    user_id      :: bitstring(),
    created_at   :: integer()
}).
