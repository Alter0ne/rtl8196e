/*
 * Copyright © 2012-2015 Michael Heimpold <mhei@heimpold.de>
 *
 * SPDX-License-Identifier: LGPL-2.1+
 */

#ifndef UGPIO_H
#define UGPIO_H

#include <stddef.h>
#include "ugpio-version.h"

#ifdef  __cplusplus
# define UGPIO_BEGIN_DECLS  extern "C" {
# define UGPIO_END_DECLS    }
#else
# define UGPIO_BEGIN_DECLS
# define UGPIO_END_DECLS
#endif

UGPIO_BEGIN_DECLS

/* taken from linux/gpio.h */
#define GPIOF_DIR_OUT                (0 << 0)
#define GPIOF_DIR_IN                 (1 << 0)

#define GPIOF_INIT_LOW               (0 << 1)
#define GPIOF_INIT_HIGH              (1 << 1)

#define GPIOF_IN                     (GPIOF_DIR_IN)
#define GPIOF_OUT_INIT_LOW           (GPIOF_DIR_OUT | GPIOF_INIT_LOW)
#define GPIOF_OUT_INIT_HIGH          (GPIOF_DIR_OUT | GPIOF_INIT_HIGH)

#define GPIOF_TRIG_FALL              (1 << 2)
#define GPIOF_TRIG_RISE              (1 << 3)
#define GPIOF_TRIGGER_MASK           (GPIOF_TRIG_FALL | GPIOF_TRIG_RISE)

#define GPIOF_REQUESTED              (1 << 4)
#define GPIOF_CLOEXEC                (1 << 5)
#define GPIOF_ALTERABLE_DIRECTION    (1 << 6)
#define GPIOF_DIRECTION_UNKNOWN      (1 << 7)
#define GPIOF_ALTERABLE_EDGE         (1 << 8)

struct gpio;
typedef struct gpio ugpio_t;

/**
 * Low level API
 */
int gpio_is_requested(unsigned int gpio);
int gpio_request(unsigned int gpio, const char *label);
int gpio_request_one(unsigned int gpio, unsigned int flags, const char *label);
int gpio_request_array(const struct gpio *array, size_t num);
int gpio_free(unsigned int gpio);
void gpio_free_array(const struct gpio *array, size_t num);

int gpio_alterable_direction(unsigned int gpio);
int gpio_get_direction(unsigned int gpio);
int gpio_direction_input(unsigned int gpio);
int gpio_direction_output(unsigned int gpio, int value);

int gpio_get_activelow(unsigned int gpio);
int gpio_set_activelow(unsigned int gpio, int value);

int gpio_get_value(unsigned int gpio);
int gpio_set_value(unsigned int gpio, int value);

int gpio_alterable_edge(unsigned int gpio);
int gpio_set_edge_str(unsigned int gpio, const char *edge);
int gpio_set_edge(unsigned int gpio, unsigned int flags);
int gpio_get_edge(unsigned int gpio);

/**
 * Higher level API
 *
 * Each GPIO is handled within a GPIO context object. Such an object encapsulates
 * all functions which can be run on a GPIO. First, you have to create such an
 * object using ugpio_request_one. Before you actually can use it, you have to
 * call ugpio_open. After usage, close the object with ugpio_close and free up
 * the used ressources with ugpio_free.
 */

/**
 * Request a GPIO context.
 *
 * This function exports a GPIO but does not configure it. It tries to detect
 * the current state of the GPIO and returns it's state in the GPIO contect.
 *
 * @param gpio the GPIO number to request
 * @param label an optional label for this GPIO
 * @return returns a ugpio_t object on success, NULL otherwise
 */
ugpio_t *ugpio_request(unsigned int gpio, const char *label);

/**
 * Request a GPIO context.
 *
 * This function exports a GPIO and configures it according to the settings
 * given in flags. If the GPIO cannot be used for the intended purpose this
 * functions fails and return NULL. Otherwise a GPIO context is returned.
 *
 * @param gpio the GPIO number to request
 * @param flags a combination of or-ed GPIOF_* constants
 * @param label an optional label for this GPIO
 * @return returns a ugpio_t object on success, NULL otherwise
 */
ugpio_t *ugpio_request_one(unsigned int gpio, unsigned int flags, const char *label);

/**
 * Release/free a GPIO context.
 *
 * @param ctx a GPIO context
 */
void ugpio_free(ugpio_t *ctx);

/**
 * Open the GPIO.
 *
 * This opens /sys/class/gpio/gpioXY/value file, but not the other file handles.
 *
 * @param ctx a GPIO context
 * @return returns the opened file descriptor, -1 on error.
 */
int ugpio_open(ugpio_t *ctx);

/**
 * Open the GPIO.
 *
 * This opens all files of the GPIO (/sys/class/gpio/gpioXY/...). Use this function e.g.
 * when you drop privileges later and want still be able to re-configure everything then.
 *
 * @param ctx a GPIO context
 * @return 0 on success, -1 on error with errno set appropriately
 */
int ugpio_full_open(ugpio_t *ctx);

/**
 * Close the GPIO.
 *
 * This closes all open file handles of the GPIO context.
 *
 * @param ctx a GPIO context
 */
void ugpio_close(ugpio_t *ctx);

/**
 * Return the GPIO context's value file descriptor.
 *
 * Using this function the application can feed the file descriptor into it's
 * select/poll... loop to monitor it for events.
 *
 * @param ctx a GPIO context
 * @return the file descriptor of the opened value file of the GPIO
 */
int ugpio_fd(ugpio_t *ctx);

/**
 * Get the GPIO context's current value.
 *
 * Read the current GPIO pin level which is 0 or 1. Usually this corresponds
 * to physical LOW and HIGH levels, but depends on 'active low' configuration
 * of the GPIO.
 *
 * @param ctx a GPIO context
 * @return 0 or 1 on success, -1 on error with errno set appropriately
 */
int ugpio_get_value(ugpio_t *ctx);

/**
 * Set the GPIO context's current value.
 *
 * Giving a value of 0 normally forces a LOW on the GPIO pin, whereas a value
 * of 1 sets the GPIO to HIGH. But note, that if the GPIO is configured as
 * 'active low' then the logic is reversed.
 *
 * @param ctx a GPIO context
 * @param value the value to set
 * @return 0 on success, -1 on error with errno set appropriately
 */
int ugpio_set_value(ugpio_t *ctx, int value);

/**
 * Get the GPIO context's active low flag.
 *
 * Read the current GPIO active_low flag which is 0 or 1. Together which the value
 * this controls the GPIO pin level.
 *
 * @param ctx a GPIO context
 * @return 0 or 1 on success, -1 on error with errno set appropriately
 */
int ugpio_get_activelow(ugpio_t *ctx);

/**
 * Set the GPIO context's active low flag.
 *
 * @param ctx a GPIO context
 * @param flag the flag to set: 1 enable the flag, 0 disables the flag
 * @return 0 on success, -1 on error with errno set appropriately
 */
int ugpio_set_activelow(ugpio_t *ctx, int flag);

/**
 * Check whether it is allowed to switch the direction of the GPIO.
 *
 * @param ctx a GPIO context
 * @return 1 when the GPIO supports this, 0 otherwise.
 */
int ugpio_alterable_direction(ugpio_t *ctx);

/**
 * Get the GPIO context's direction.
 *
 * Read the current GPIO direction and return GPIOF_DIR_IN or GIOF_DIR_OUT.
 *
 * @param ctx a GPIO context
 * @return GPIOF_DIR_IN or GPIOF_DIR_OUT on success, -1 on error with errno
 *         set appropriately
 */
int ugpio_get_direction(ugpio_t *ctx);

/**
 * Set the GPIO context's direction to input.
 *
 * @param ctx a GPIO context
 * @return 0 on success, -1 on error with errno set appropriately
 */
int ugpio_direction_input(ugpio_t *ctx);

/**
 * Set the GPIO context's direction to output.
 *
 * @param ctx a GPIO context
 * @param value the value to set
 * @return 0 on success, -1 on error with errno set appropriately
 */
int ugpio_direction_output(ugpio_t *ctx, int value);

/**
 * Check whether the GPIO can generate IRQs on input and therefore
 * supports the edge api.
 *
 * @param ctx a GPIO context
 * @return 1 when the GPIO supports this, 0 otherwise.
 */
int ugpio_alterable_edge(ugpio_t *ctx);

/**
 * Get the GPIO context's trigger flags.
 *
 * Read the current GPIO trigger flags and returns the corresponding
 * GPIOF_TRIG_* flags.
 *
 * @param ctx a GPIO context
 * @return 0 or one or more GPIOF_TRIG_* or-ed together, -1 on error
 *         with errno set appropriately:
 *         EFAULT - the flag string found could not be mappend to the
 *         library flags.
 */
int ugpio_get_edge(ugpio_t *ctx);

/**
 * Set the GPIO context's trigger flags.
 *
 * @param ctx a GPIO context
 * @param flags the flag to set: 0 or one or more GPIOF_TRIG_* flags
 *        or-ed together
 * @return 0 on success, -1 on error with errno set appropriately:
 *         EINVAL - the flags could not be mapped to any known flag string.
 */
int ugpio_set_edge(ugpio_t *ctx, int flags);

UGPIO_END_DECLS

#endif  /* UGPIO_H */
