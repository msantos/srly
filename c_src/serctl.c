/* Copyright (c) 2011-2012, Michael Santos <michael.santos@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include "erl_nif.h"
#include "erl_driver.h"
#include "serctl.h"


static ERL_NIF_TERM error_tuple(ErlNifEnv *env, int errnum);
void srly_state_free(ErlNifEnv *env, void *obj);

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_undefined;

static ErlNifResourceType *SRLY_STATE_RESOURCE;

typedef struct _srly_state {
    int fd;
} SRLY_STATE;


    static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_eagain = enif_make_atom(env, "eagain");
    atom_undefined = enif_make_atom(env, "undefined");

    if ( (SRLY_STATE_RESOURCE = enif_open_resource_type(env, NULL,
        "srly_state_resource", srly_state_free,
        ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    return (0);
}

    static void
unload(ErlNifEnv* env, void *priv_data)
{
}

    static ERL_NIF_TERM
nif_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char buf[MAXPATHLEN];
    SRLY_STATE *sp = NULL;


    if (enif_get_string(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1) < 1)
        return (-1);

    sp = enif_alloc_resource(SRLY_STATE_RESOURCE, sizeof(SRLY_STATE));
    if (sp == NULL)
        return error_tuple(env, ENOMEM);

    sp->fd = open(buf, O_RDWR|O_NOCTTY|O_NONBLOCK);

    if (sp->fd < 0) {
        enif_release_resource(sp);
        return error_tuple(env, errno);
    }

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_resource(env, sp));
}

    static ERL_NIF_TERM
nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SRLY_STATE *sp = NULL;
    ERL_NIF_TERM rv = atom_ok;


    if (!enif_get_resource(env, argv[0], SRLY_STATE_RESOURCE, (void **)&sp))
        return enif_make_badarg(env);

    if (close(sp->fd) < 0)
        rv = error_tuple(env, errno);

    sp->fd = -1;
    enif_release_resource(sp);

    return rv;
}

    static ERL_NIF_TERM
nif_read(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SRLY_STATE *sp = NULL;
    unsigned long len = 0;

    ErlNifBinary buf = {0};
    ssize_t bufsz = 0;


    if (!enif_get_resource(env, argv[0], SRLY_STATE_RESOURCE, (void **)&sp))
        return enif_make_badarg(env);

    if (!enif_get_ulong(env, argv[1], &len))
        return enif_make_badarg(env);

    if (!enif_alloc_binary(len, &buf))
        return error_tuple(env, ENOMEM);

    if ( (bufsz = read(sp->fd, buf.data, buf.size)) == -1) {
        int err = errno;
        enif_release_binary(&buf);
        return error_tuple(env, err);
    }

    if (bufsz < buf.size && !enif_realloc_binary(&buf, bufsz)) {
        enif_release_binary(&buf);
        return error_tuple(env, ENOMEM);
    }

    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &buf));
}

    static ERL_NIF_TERM
nif_write(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SRLY_STATE *sp = NULL;
    ErlNifBinary buf = {0};


    if (!enif_get_resource(env, argv[0], SRLY_STATE_RESOURCE, (void **)&sp))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &buf))
        return enif_make_badarg(env);

    if (write(sp->fd, buf.data, buf.size) == -1)
        return error_tuple(env, errno);

    return atom_ok;
}

    static ERL_NIF_TERM
nif_tcgetattr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SRLY_STATE *sp = NULL;
    ErlNifBinary buf = {0};

    int err = 0;


    if (!enif_get_resource(env, argv[0], SRLY_STATE_RESOURCE, (void **)&sp))
        return enif_make_badarg(env);

    if (!enif_alloc_binary(sizeof(struct termios), &buf))
        return error_tuple(env, ENOMEM);

    if (tcgetattr(sp->fd, (struct termios *)buf.data) < 0) {
        err = errno;
        enif_release_binary(&buf);
        return error_tuple(env, err);
    }

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_binary(env, &buf));
}

    static ERL_NIF_TERM
nif_tcsetattr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SRLY_STATE *sp = NULL;
    int opt = 0;

    ErlNifBinary buf = {0};


    if (!enif_get_resource(env, argv[0], SRLY_STATE_RESOURCE, (void **)&sp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &opt))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[2], &buf) || (buf.size != sizeof(struct termios)))
        return enif_make_badarg(env);

    if (tcsetattr(sp->fd, opt, (struct termios *)buf.data) < 0)
        return error_tuple(env, errno);

    return atom_ok;
}

    static ERL_NIF_TERM
nif_cfsetispeed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary buf = {0};
    unsigned int speed = 0;


    if (!enif_inspect_binary(env, argv[0], &buf) || (buf.size != sizeof(struct termios)))
        return enif_make_badarg(env);

    if (!enif_get_uint(env, argv[1], &speed))
        return enif_make_badarg(env);

    if (!enif_realloc_binary(&buf, buf.size))
        return error_tuple(env, ENOMEM);

    if (cfsetispeed((struct termios *)buf.data, speed) < 0)
        return error_tuple(env, errno);

    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &buf));
}

    static ERL_NIF_TERM
nif_cfsetospeed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary buf = {0};
    unsigned int speed = 0;


    if (!enif_inspect_binary(env, argv[0], &buf) || (buf.size != sizeof(struct termios)))
        return enif_make_badarg(env);

    if (!enif_get_uint(env, argv[1], &speed))
        return enif_make_badarg(env);

    if (!enif_realloc_binary(&buf, buf.size))
        return error_tuple(env, ENOMEM);

    if (cfsetospeed((struct termios *)buf.data, speed) < 0)
        return error_tuple(env, errno);

    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &buf));
}

    static ERL_NIF_TERM
nif_getfd(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    SRLY_STATE *sp = NULL;


    if (!enif_get_resource(env, argv[0], SRLY_STATE_RESOURCE, (void **)&sp))
        return enif_make_badarg(env);

    return enif_make_int(env, sp->fd);
}

    static ERL_NIF_TERM
nif_constants(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const struct SERCTL_DEF *p = NULL;
    ERL_NIF_TERM list = {0};


    list = enif_make_list(env, 0);

    for (p = serctl_const; p->key != NULL; p++) {
        list = enif_make_list_cell(
                env,
                enif_make_tuple2(
                    env,
                    enif_make_atom(env, p->key),
                    enif_make_uint(env, p->val)
                    ),
                list);
    }

    return list;
}

    static ERL_NIF_TERM
nif_constant(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char buf[255];
    const struct SERCTL_DEF *p = NULL;


    if (enif_get_atom(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    for (p = serctl_const; p->key != NULL; p++) {
        if (!strcmp(buf, p->key))
            return enif_make_uint(env, p->val);
    }

    return atom_undefined;
}

    void
srly_state_free(ErlNifEnv *env, void *obj)
{
    SRLY_STATE *p = obj;

    if (p->fd < 0)
        return;

    (void)close(p->fd);
    p->fd = -1;
}

    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, int errnum)
{
    return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errnum)));
}


static ErlNifFunc nif_funcs[] = {
    {"open", 1, nif_open},
    {"close", 1, nif_close},
    {"read", 2, nif_read},
    {"write", 2, nif_write},
    {"tcgetattr", 1, nif_tcgetattr},
    {"tcsetattr", 3, nif_tcsetattr},
    {"cfsetispeed", 2, nif_cfsetispeed},
    {"cfsetospeed", 2, nif_cfsetospeed},

    {"getfd", 1, nif_getfd},
    {"constant", 0, nif_constants},
    {"constant", 1, nif_constant}
};

ERL_NIF_INIT(serctl, nif_funcs, load, NULL, NULL, unload)
