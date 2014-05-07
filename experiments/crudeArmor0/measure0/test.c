/******************************************************************************
 * test.c
 * 
 * Test code for all the various frontends; split from kernel.c
 * 
 * Copyright (c) 2002-2003, K A Fraser & R Neugebauer
 * Copyright (c) 2005, Grzegorz Milos, Intel Research Cambridge
 * Copyright (c) 2006, Robert Kaiser, FH Wiesbaden
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
 * DEALINGS IN THE SOFTWARE.
 */

// The LIBXL_API_VERSION is needed/used by the libxl.h include file.
#define LIBXL_API_VERSION 0x040200

#include <mini-os/os.h>
#include <mini-os/hypervisor.h>
#include <mini-os/mm.h>
#include <mini-os/events.h>
#include <mini-os/time.h>
#include <mini-os/types.h>
#include <mini-os/lib.h>
#include <mini-os/sched.h>
#include <mini-os/gnttab.h>
#include <mini-os/xmalloc.h>
#include <fcntl.h>
#include <xen/features.h>
#include <xen/version.h>

#include <libxl_utils.h>

// Logger for XL
static xentoollog_logger_stdiostream *g_logger;
static libxl_ctx * g_ctx;

/**
 * This is the test thread that runns every 1 seconds.
 **/
static void periodic_thread(void *p)
{
    int libxlStatus;
    struct timeval tv;
    int minmsglevel = 0; // All messages.

    printk("Periodic thread started.\n");

    // Do some initialization.

    g_logger = xtl_createlogger_stdiostream(stderr, minmsglevel,  0);
    if(!g_logger) {
      printk("!!! Failed to create the logger.");
      printk("!!! Exiting periodic thread");
      return;
    }

    libxlStatus = libxl_ctx_alloc(&g_ctx, LIBXL_VERSION, 0, (xentoollog_logger*)g_logger);
    if (libxlStatus != 0) {
      printk("!!! Status from 'libxl_ctx_alloc' = %d", libxlStatus);
      printk("!!! Exiting periodic thread");
      return;
    }


    for(;;)
    {
        gettimeofday(&tv, NULL);
        printk("T(s=%ld us=%ld)\n", tv.tv_sec, tv.tv_usec);

        // -----------------------------------------------------
        // At some point we could probably save some code memory (.a lib size)
        // by using the low level libxc.
        char targetDomName[] = "CentOS65_x86_64";

        printk("Looking for target domain named '%s'", targetDomId);

	// Convert the domain name to an ID
        libxl_domid targetDomId; // domain id

        // get the context.


        // Tell the console what domain name we are looking for so that 
        // they can give the target domain the same name.
	libxlStatus = libxl_name_to_domid(libxl_ctx *ctx, targetDomName, &targetDomId);

        printk("libxl_name_todomid status=%d", libxlStatus);

        libxlStatus = libxl_ctx_free( ctx );

        //-------------------------

        msleep(1000);
    }
}

//----------------------------------------------------------------------------

static struct kbdfront_dev *kbd_dev;
static void kbdfront_thread(void *p)
{
    DEFINE_WAIT(w);
    DEFINE_WAIT(w2);
    int x = WIDTH / 2, y = HEIGHT / 2, z = 0;

    kbd_dev = init_kbdfront(NULL, 1);
    if (!kbd_dev)
        return;

    down(&fbfront_sem);
    refresh_cursor(x, y);
    while (1) {
        union xenkbd_in_event kbdevent;
        union xenfb_in_event fbevent;
        int sleep = 1;

        add_waiter(w, kbdfront_queue);
        add_waiter(w2, fbfront_queue);

        while (kbdfront_receive(kbd_dev, &kbdevent, 1) != 0) {
            sleep = 0;
            switch(kbdevent.type) {
            case XENKBD_TYPE_MOTION:
                printk("motion x:%d y:%d z:%d\n",
                        kbdevent.motion.rel_x,
                        kbdevent.motion.rel_y,
                        kbdevent.motion.rel_z);
                x += kbdevent.motion.rel_x;
                y += kbdevent.motion.rel_y;
                z += kbdevent.motion.rel_z;
                clip_cursor(&x, &y);
                refresh_cursor(x, y);
                break;
            case XENKBD_TYPE_POS:
                printk("pos x:%d y:%d dz:%d\n",
                        kbdevent.pos.abs_x,
                        kbdevent.pos.abs_y,
                        kbdevent.pos.rel_z);
                x = kbdevent.pos.abs_x;
                y = kbdevent.pos.abs_y;
                z = kbdevent.pos.rel_z;
                clip_cursor(&x, &y);
                refresh_cursor(x, y);
                break;
            case XENKBD_TYPE_KEY:
                printk("key %d %s\n",
                        kbdevent.key.keycode,
                        kbdevent.key.pressed ? "pressed" : "released");
                if (kbdevent.key.keycode == BTN_LEFT) {
                    printk("mouse %s at (%d,%d,%d)\n",
                            kbdevent.key.pressed ? "clic" : "release", x, y, z);
                    if (kbdevent.key.pressed) {
                        uint32_t color = rand();
                        fbfront_drawvert(x - 16, y - 16, y + 15, color);
                        fbfront_drawhoriz(x - 16, x + 15, y + 16, color);
                        fbfront_drawvert(x + 16, y - 15, y + 16, color);
                        fbfront_drawhoriz(x - 15, x + 16, y - 16, color);
                        fbfront_update(fb_dev, x - 16, y - 16, 33, 33);
                    }
                } else if (kbdevent.key.keycode == KEY_Q) {
                    struct sched_shutdown sched_shutdown = { .reason = SHUTDOWN_poweroff };
                    HYPERVISOR_sched_op(SCHEDOP_shutdown, &sched_shutdown);
                    do_exit();
                }
                break;
            }
        }
        while (fbfront_receive(fb_dev, &fbevent, 1) != 0) {
            sleep = 0;
            switch(fbevent.type) {
            case XENFB_TYPE_REFRESH_PERIOD:
                refresh_period = fbevent.refresh_period.period;
                printk("refresh period %d\n", refresh_period);
                refresh_cursor(x, y);
                break;
            }
        }
        if (sleep)
            schedule();
    }
}


int app_main(start_info_t *si)
{
    printk("Test main: start_info=%p\n", si);
    create_thread("periodic_thread", periodic_thread, si);
    create_thread("kbdfront", kbdfront_thread, si);
    return 0;
}

void shutdown_frontends(void)
{
    if (kbd_dev)
        shutdown_kbdfront(kbd_dev);

}
