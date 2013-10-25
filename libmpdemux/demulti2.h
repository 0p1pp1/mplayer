/*
 *  MULTI2 Descrambling Library for BCAS
 *
 *  Copyright (C) 2013 0p1pp1
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.=
 */
#ifndef _DEMULTI2_H_
#define _DEMULTI2_H_

#include <inttypes.h>

#ifdef __cplusplus
extern "C"
{
#endif

typedef struct demulti2_context * Demulti2Handle;

/*
 * error codes for API functions
 */
#define DEMULTI2_RET_OK		0	/*!< operation succeeded. */
#define DEMULTI2_E_INV_HANDLE	1	/*!< an invalid handle was passed. */
#define DEMULTI2_E_INV_ARG	2	/*!< one of the arguments has an invalid value. */
#define DEMULTI2_E_MEM		3	/*!< out of memory error. */
#define DEMULTI2_E_NOT_READY	4	/*!< required ECM has not been fed or decoded yet. */
#define DEMULTI2_E_NO_CARD	5	/*!< BCAS Card is not inserted/ready. */
#define DEMULTI2_E_CARD_IO	6	/*!< I/O to BCAS failed. */
#define DEMULTI2_E_BAD_RESP	7	/*!< got a bad response from the card. */
/*!
 * \brief Initialize the library and a BCAS card, and create a session.
 *
 * Users must call this function first before any other functions.
 *
 * \note
 * These environment varibales are read to control details.
 * - \c DEMULTI2_MODE : (case sensitive) name of the descrambling method to use
 *  + \c 'yakisoba' : use direct software descrambling with libyakisoba
 *  + \c 'sobacas'  : use an emulated BCAS card with libsobacas
 *  + \c 'pcsc'     : use a real BCAS card with PC/SC
 *  + \c 'auto' / undef : detect available libraries out of the above three
 *   and select one in the order of yakisoba -> sobacas -> pcsc
 * - \c DEMULTI2_CARD : the name of the card to use
 *  (for \c DEMULTI2_MODE == pcsc only)
 * \return a handle to the newly created context for this session
 */
extern Demulti2Handle demulti2_open(void);

/*!
 * \brief Close the session and release the associated resources.
 *
 * After this call, no functions should be called with the handle \a h.
 * \param h library context handle
 */
extern void demulti2_close(Demulti2Handle h);

/*!
 * \brief Descramble a TS packet's payload.
 * \param h library context handle
 * \param [in] src a pointer to the _payload_ (excluding header and AFC)
 * \param len length of the payload
 * \param sc the 4th byte of the packet header, including scrambling control flag
 * \param ecm_pid PID of the ECM stream for this packet
 * \param [out] dst a pointer to a buffer for the descrambled data.
 *        \note set \a src or \c NULL to \a dst for inline descrambling.
 * \return error code (> 0) if failed or 0 otherwise
 */
extern int demulti2_descramble(Demulti2Handle h, const uint8_t *src, int len,
	uint8_t sc, uint16_t ecm_pid, uint8_t *dst);

/*!
 * \brief Feed an ECM body to BCAS system.
 * \param h library context handle
 * \param [in] src a pointer to the ECM body
 * \param len length of the ECM body
 * \param ecm_pid PID of this ECM stream
 * \return error code (> 0) if failed or 0 otherwise
 */
extern int demulti2_feed_ecm(Demulti2Handle h, const uint8_t *body, int len,
	uint16_t ecm_pid);

/*
 * TODO: support EMM, EMM messages, commands from EMM responses
 */

#ifdef __cplusplus
}
#endif

#endif /* _DEMULTI2_H_ */
