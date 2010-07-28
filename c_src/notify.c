/* Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
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
#include <string.h>
#include <libnotify/notify.h>

#include "erl_nif.h"


static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_nomem;

static gchar *stralloc(ErlNifBinary *bin);
void strfree(gchar *p);


    static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    char buf[1024];

    if (enif_get_string(env, load_info, buf, sizeof(buf), ERL_NIF_LATIN1) < 1)
        return (-1);

    if (!notify_init(buf))
        return (-1);

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_nomem = enif_make_atom(env, "enomem");

    return (0);
}

    void
unload(ErlNifEnv *env, void *priv_data)
{
    notify_uninit();
}


    static ERL_NIF_TERM
nif_notify(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary summary;
    ErlNifBinary body;
    ErlNifBinary icon;

    ErlNifBinary category;
    int urgency = NOTIFY_URGENCY_NORMAL;
    int timeout = 0;

    gchar *s_summary = NULL;
    gchar *s_body = NULL;
    gchar *s_icon = NULL;
    gchar *s_category = NULL;

    NotifyNotification *notify = NULL;

    ERL_NIF_TERM rv = atom_ok;


    if (!enif_inspect_iolist_as_binary(env, argv[0], &summary))
        return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[1], &body))
        return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[2], &icon))
        return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[3], &category))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[4], &urgency))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[5], &timeout))
        return enif_make_badarg(env);

    s_summary = stralloc(&summary);
    s_body = stralloc(&body);
    s_icon = stralloc(&icon);
    s_category = stralloc(&category);

    if ( (s_summary == NULL) || (s_body == NULL) ||
            (s_icon == NULL) || (s_category == NULL)) {
        rv = atom_nomem;
        goto ERR;
    }

    notify = notify_notification_new(s_summary, s_body, s_icon, NULL);

    notify_notification_set_category(notify, s_category);
    notify_notification_set_urgency(notify, urgency);
    notify_notification_set_timeout(notify, timeout);

    notify_notification_show(notify, NULL);

    g_object_unref(G_OBJECT(notify));

ERR:
    strfree(s_summary);
    strfree(s_body);
    strfree(s_icon);
    strfree(s_category);

    return rv;
}


    static gchar *
stralloc(ErlNifBinary *bin)
{
    gchar *str = NULL;

    str = (gchar *)calloc(bin->size+1, sizeof(gchar));
    if (str == NULL)
        return (NULL);

    (void)memcpy(str, bin->data, bin->size);
    return (str);
}


    void
strfree(gchar *p)
{
    if (p)
        free(p);
}


static ErlNifFunc nif_funcs[] = {
    {"notify", 6, nif_notify}
};

ERL_NIF_INIT(notify, nif_funcs, load, NULL, NULL, unload)


