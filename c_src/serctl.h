/* Copyright (c) 2011-2014, Michael Santos <michael.santos@gmail.com>
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <err.h>
#include <sys/param.h>
#include <termios.h>
#include <sys/ioctl.h>

#include <errno.h>

#define SRLY_VERSION   "0.01"

struct SERCTL_DEF {
    const char *key;
    u_int32_t val;
};

const struct SERCTL_DEF serctl_const[] = {
    {"nccs", NCCS},

    {"vintr", VINTR},
    {"vquit", VQUIT},
    {"verase", VERASE},
    {"vkill", VKILL},
    {"veof", VEOF},
    {"vtime", VTIME},
    {"vmin", VMIN},
#ifdef VSWTC
    {"vswtc", VSWTC},
#endif
    {"vstart", VSTART},
    {"vstop", VSTOP},
    {"vsusp", VSUSP},
    {"veol", VEOL},
    {"vreprint", VREPRINT},
    {"vdiscard", VDISCARD},
    {"vwerase", VWERASE},
    {"vlnext", VLNEXT},
    {"veol2", VEOL2},
    {"ignbrk", IGNBRK},
    {"brkint", BRKINT},
    {"ignpar", IGNPAR},
    {"parmrk", PARMRK},
    {"inpck", INPCK},
    {"istrip", ISTRIP},
    {"inlcr", INLCR},
    {"igncr", IGNCR},
    {"icrnl", ICRNL},
#ifdef IUCLC
    {"iuclc", IUCLC},
#endif
    {"ixon", IXON},
    {"ixany", IXANY},
    {"ixoff", IXOFF},
    {"imaxbel", IMAXBEL},
    {"iutf8", IUTF8},
    {"opost", OPOST},
#ifdef OLCUC
    {"olcuc", OLCUC},
#endif
    {"onlcr", ONLCR},
    {"ocrnl", OCRNL},
    {"onocr", ONOCR},
    {"onlret", ONLRET},
    {"ofill", OFILL},
    {"ofdel", OFDEL},
    {"b0", B0},
    {"b50", B50},
    {"b75", B75},
    {"b110", B110},
    {"b134", B134},
    {"b150", B150},
    {"b200", B200},
    {"b300", B300},
    {"b600", B600},
    {"b1200", B1200},
    {"b1800", B1800},
    {"b2400", B2400},
    {"b4800", B4800},
    {"b9600", B9600},
    {"b19200", B19200},
    {"b38400", B38400},
    {"csize", CSIZE},
    {"cs5", CS5},
    {"cs6", CS6},
    {"cs7", CS7},
    {"cs8", CS8},
    {"cstopb", CSTOPB},
    {"cread", CREAD},
    {"parenb", PARENB},
    {"parodd", PARODD},
    {"hupcl", HUPCL},
    {"clocal", CLOCAL},
    {"b57600", B57600},
    {"b115200", B115200},
    {"b230400", B230400},
#ifdef B460800
    {"b460800", B460800},
#endif
#ifdef B500000
    {"b500000", B500000},
#endif
#ifdef B576000
    {"b576000", B576000},
#endif
#ifdef B921600
    {"b921600", B921600},
#endif
#ifdef B1000000
    {"b1000000", B1000000},
#endif
#ifdef B1152000
    {"b1152000", B1152000},
#endif
    {"crtscts", CRTSCTS},
    {"isig", ISIG},
    {"icanon", ICANON},
    {"echo", ECHO},
    {"echoe", ECHOE},
    {"echok", ECHOK},
    {"echoke", ECHOKE},
    {"echonl", ECHONL},
    {"noflsh", NOFLSH},
    {"tostop", TOSTOP},
    {"iexten", IEXTEN},
    {"tcooff", TCOOFF},
    {"tcoon", TCOON},
    {"tcioff", TCIOFF},
    {"tcion", TCION},
    {"tciflush", TCIFLUSH},
    {"tcoflush", TCOFLUSH},
    {"tcioflush", TCIOFLUSH},
    {"tcsanow", TCSANOW},
    {"tcsadrain", TCSADRAIN},
    {"tcsaflush", TCSAFLUSH},

#ifdef TIOCMBIC
    {"tiocmbic", TIOCMBIC},
#endif
#ifdef TIOCMBIS
    {"tiocmbis", TIOCMBIS},
#endif
#ifdef TIOCMGET
    {"tiocmget", TIOCMGET},
#endif
#ifdef TIOCM_DTR
    {"tiocm_dtr", TIOCM_DTR},
#endif
#ifdef TIOCM_RTS
    {"tiocm_rts", TIOCM_RTS},
#endif

    {NULL, 0}
};
