--
-- PostgreSQL database dump
--

-- Dumped from database version 11.1
-- Dumped by pg_dump version 11.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

DROP INDEX public.users_uuid_idx;
DROP INDEX public.users_username_idx;
DROP INDEX public.users_email_idx;
ALTER TABLE ONLY public.users DROP CONSTRAINT users_pkey;
ALTER TABLE public.users ALTER COLUMN id DROP DEFAULT;
DROP SEQUENCE public.users_id_seq;
DROP TABLE public.users;
DROP EXTENSION pgcrypto;
--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: users; Type: TABLE; Schema: public; Owner: shortcut
--

CREATE TABLE public.users (
    id integer NOT NULL,
    uuid uuid DEFAULT public.gen_random_uuid() NOT NULL,
    username character varying(40) NOT NULL,
    email character varying(100) NOT NULL,
    password text NOT NULL
);


ALTER TABLE public.users OWNER TO shortcut;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: shortcut
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO shortcut;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: shortcut
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: shortcut
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: shortcut
--

COPY public.users (id, uuid, username, email, password) FROM stdin;
\.


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: shortcut
--

SELECT pg_catalog.setval('public.users_id_seq', 1, false);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: shortcut
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users_email_idx; Type: INDEX; Schema: public; Owner: shortcut
--

CREATE INDEX users_email_idx ON public.users USING btree (email);


--
-- Name: users_username_idx; Type: INDEX; Schema: public; Owner: shortcut
--

CREATE INDEX users_username_idx ON public.users USING btree (username);


--
-- Name: users_uuid_idx; Type: INDEX; Schema: public; Owner: shortcut
--

CREATE INDEX users_uuid_idx ON public.users USING btree (uuid);


--
-- PostgreSQL database dump complete
--

