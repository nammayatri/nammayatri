package in.juspay.mobility.sdk.security;


import java.util.Arrays;


/**
 * @author Tim McLean
 * @author others
 * @version 2018-07-23
 */
public class Base64Codec {


    /**
     * Computes the base 64 encoded character length for the specified
     * input byte length.
     *
     * @param inputLength The input byte length.
     * @param urlSafe     {@code true} for URL-safe encoding.
     *
     * @return The base 64 encoded character length.
     */
    static int computeEncodedLength(final int inputLength, final boolean urlSafe) {

        if (inputLength == 0) {
            return 0;
        }

        if (urlSafe) {

            // Compute the number of complete quads (4-char blocks)
            int fullQuadLength = (inputLength / 3) << 2;

            // Compute the remaining bytes at the end
            int remainder = inputLength % 3;

            // Compute the total
            return remainder == 0 ? fullQuadLength : fullQuadLength + remainder + 1;
        } else {
            // Original Mig code
            return ((inputLength - 1) / 3 + 1) << 2;
        }
    }


    // *** Timing-protected (tp) utility methods ***
    // This class is used to encode/decode private keys, so we make an effort
    // to prevent side channel leaks. Here we define a number of timing leak
    // resistant utility methods. Boolean values are stored in ints in order to
    // prevent optimizations that would reintroduce timing leaks.
    //
    // Some background information on preventing side channel leaks:
    // - https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html

    /**
     * Select one of two values based on {@code bool} without leaking
     * information about which one was selected.
     *
     * @param bool Must be 1 or 0
     * @param when_true The value to return if {@code bool} is 1
     * @param when_false The value to return if {@code bool} is 0
     * @return Either when_true or when_false
     */
    static int tpSelect(int bool, int when_true, int when_false) {

        // Will be 0x00000000 when bool == 1, or 0xFFFFFFFF when bool == 0
        final int mask = bool - 1;

        return when_true ^ (mask & (when_true ^ when_false));
    }


    /**
     * Checks if a &lt; b without leaking information about either a or b
     *
     * @param a Any int
     * @param b Any int
     * @return 1 if yes, 0 if no
     */
    static int tpLT(int a, int b) {

        return (int) (((long) a - (long) b) >>> 63);
    }


    /**
     * Checks if a &gt; b without leaking information about either a or b
     *
     * @param a Any int
     * @param b Any int
     * @return 1 if yes, 0 if no
     */
    static int tpGT(int a, int b) {

        return (int) (((long) b - (long) a) >>> 63);
    }


    /**
     * Checks if a == b without leaking information about either a or b
     *
     * @param a Any int
     * @param b Any int
     * @return 1 if yes, 0 if no
     */
    static int tpEq(int a, int b) {

        // This is magic but it will make sense
        // if you think about it for 30 minutes

        final int bit_diff = a ^ b;
        final int msb_iff_zero_diff = (bit_diff - 1) & (~bit_diff);
        return msb_iff_zero_diff >>> 63;
    }


    /**
     * Convert a digit index to the appropriate base 64 ASCII byte.
     *
     * Uses '+' and '/' for 62 and 63, as required for standard base 64.
     *
     * @param digit_idx Must be at least 0 and at most 63. Output is undefined
     *                  if digit_idx is not on this range.
     * @return An ASCII character
     */
    static byte encodeDigitBase64(int digit_idx) {

        assert digit_idx >= 0 && digit_idx <= 63;

        // Figure out which type of digit this should be
        final int is_uppercase = tpLT(digit_idx, 26);
        final int is_lowercase = tpGT(digit_idx, 25) & tpLT(digit_idx, 52);
        final int is_decimal   = tpGT(digit_idx, 51) & tpLT(digit_idx, 62);
        final int is_62        = tpEq(digit_idx, 62);
        final int is_63        = tpEq(digit_idx, 63);

        // Translate from digit index to ASCII for each hypothetical scenario
        final int as_uppercase = digit_idx -  0 + 65;
        final int as_lowercase = digit_idx - 26 + 97;
        final int as_decimal   = digit_idx - 52 + 48;
        final int as_62        = (int) '+';
        final int as_63        = (int) '/';

        // Zero out all scenarios except for the right one, and combine
        final int ascii =
                tpSelect(is_uppercase, as_uppercase, 0) |
                        tpSelect(is_lowercase, as_lowercase, 0) |
                        tpSelect(is_decimal  , as_decimal  , 0) |
                        tpSelect(is_62       , as_62       , 0) |
                        tpSelect(is_63       , as_63       , 0);

        return (byte) ascii;
    }


    /**
     * Convert a digit index to the appropriate base64url ASCII byte.
     *
     * Uses '-' and '_' for 62 and 63, as required for the base64url encoding.
     *
     * @param digit_idx Must be at least 0 and at most 63. Output is undefined
     *                  if digit_idx is not on this range.
     * @return An ASCII character
     */
    static byte encodeDigitBase64URL(int digit_idx) {

        assert digit_idx >= 0 && digit_idx <= 63;

        // Figure out which type of digit this should be
        final int is_uppercase = tpLT(digit_idx, 26);
        final int is_lowercase = tpGT(digit_idx, 25) & tpLT(digit_idx, 52);
        final int is_decimal   = tpGT(digit_idx, 51) & tpLT(digit_idx, 62);
        final int is_62        = tpEq(digit_idx, 62);
        final int is_63        = tpEq(digit_idx, 63);

        // Translate from digit index to ASCII for each hypothetical scenario
        final int as_uppercase = digit_idx -  0 + 65;
        final int as_lowercase = digit_idx - 26 + 97;
        final int as_decimal   = digit_idx - 52 + 48;
        final int as_62        = (int) '-';
        final int as_63        = (int) '_';

        // Zero out all scenarios except for the right one, and combine
        final int ascii =
                tpSelect(is_uppercase, as_uppercase, 0) |
                        tpSelect(is_lowercase, as_lowercase, 0) |
                        tpSelect(is_decimal  , as_decimal  , 0) |
                        tpSelect(is_62       , as_62       , 0) |
                        tpSelect(is_63       , as_63       , 0);

        return (byte) ascii;
    }


    /**
     * Decode an ASCII byte to a base 64 digit index (0 to 63), or -1 if the
     * input is not a valid base 64 digit.
     *
     * Supports '+' and '/' for standard base 64, but also '-' and '_' for
     * base64url.
     *
     * @param ascii An ASCII character.
     * @return A digit index i such that 0 <= i <= 63, or -1 if the input was not a digit.
     */
    static int decodeDigit(byte ascii) {

        // Figure out which type of digit this is
        final int is_uppercase = tpGT(ascii, 64) & tpLT(ascii, 91);
        final int is_lowercase = tpGT(ascii, 96) & tpLT(ascii, 123);
        final int is_decimal   = tpGT(ascii, 47) & tpLT(ascii, 58);
        final int is_62        = tpEq(ascii, (int) '-') | tpEq(ascii, (int) '+');
        final int is_63        = tpEq(ascii, (int) '_') | tpEq(ascii, (int) '/');

        // It should be one of the five categories
        final int is_valid = is_uppercase | is_lowercase | is_decimal | is_62 | is_63;

        // Translate from ASCII to digit index for each hypothetical scenario
        final int from_uppercase = ascii - 65 +  0;
        final int from_lowercase = ascii - 97 + 26;
        final int from_decimal   = ascii - 48 + 52;
        final int from_62        = 62;
        final int from_63        = 63;

        // Zero out all scenarios except for the right one, and combine
        final int digit_idx =
                tpSelect(is_uppercase, from_uppercase, 0) |
                        tpSelect(is_lowercase, from_lowercase, 0) |
                        tpSelect(is_decimal  , from_decimal  , 0) |
                        tpSelect(is_62       , from_62       , 0) |
                        tpSelect(is_63       , from_63       , 0) |
                        tpSelect(is_valid    , 0             , -1);

        assert digit_idx >= -1 && digit_idx <= 63;

        return digit_idx;
    }


    /**
     * Encodes a byte array into a base 64 encoded string.
     *
     * @param byteArray The bytes to convert. If {@code null} or length 0
     *                  an empty array will be returned.
     * @param urlSafe   If {@code true} to apply URL-safe encoding (padding
     *                  still included and not to spec).
     *
     * @return The base 64 encoded string. Never {@code null}.
     */
    public static String encodeToString(byte[] byteArray, final boolean urlSafe) throws Exception {

        // Check special case
        final int sLen = byteArray != null ? byteArray.length : 0;

        if (sLen == 0) {
            return "";
        }

        final int eLen = (sLen / 3) * 3;                      // Length of even 24-bits.
        final int dLen = computeEncodedLength(sLen, urlSafe); // Returned byte count
        final byte[] out = new byte[dLen];

        // Encode even 24-bits
        for (int s = 0, d = 0; s < eLen; ) {

            // Copy next three bytes into lower 24 bits of int, paying attention to sign
            final int i = (byteArray[s++] & 0xff) << 16 | (byteArray[s++] & 0xff) << 8 | (byteArray[s++] & 0xff);

            // Encode the int into four chars
            if (urlSafe) {
                out[d++] = encodeDigitBase64URL((i >>> 18) & 0x3f);
                out[d++] = encodeDigitBase64URL((i >>> 12) & 0x3f);
                out[d++] = encodeDigitBase64URL((i >>> 6) & 0x3f);
                out[d++] = encodeDigitBase64URL(i & 0x3f);
            } else {
                out[d++] = encodeDigitBase64((i >>> 18) & 0x3f);
                out[d++] = encodeDigitBase64((i >>> 12) & 0x3f);
                out[d++] = encodeDigitBase64((i >>> 6) & 0x3f);
                out[d++] = encodeDigitBase64(i & 0x3f);
            }
        }

        // Pad and encode last bits if source isn't even 24 bits
        // according to URL-safe switch
        final int left = sLen - eLen; // 0 - 2.
        if (left > 0) {
            // Prepare the int
            final int i = ((byteArray[eLen] & 0xff) << 10) | (left == 2 ? ((byteArray[sLen - 1] & 0xff) << 2) : 0);

            // Set last four chars
            if (urlSafe) {

                if (left == 2) {
                    out[dLen - 3] = encodeDigitBase64URL(i >> 12);
                    out[dLen - 2] = encodeDigitBase64URL((i >>> 6) & 0x3f);
                    out[dLen - 1] = encodeDigitBase64URL(i & 0x3f);
                } else {
                    out[dLen - 2] = encodeDigitBase64URL(i >> 12);
                    out[dLen - 1] = encodeDigitBase64URL((i >>> 6) & 0x3f);
                }
            } else {
                // Original Mig code with padding
                out[dLen - 4] = encodeDigitBase64(i >> 12);
                out[dLen - 3] = encodeDigitBase64((i >>> 6) & 0x3f);
                out[dLen - 2] = left == 2 ? encodeDigitBase64(i & 0x3f) : (byte) '=';
                out[dLen - 1] = (byte) '=';
            }
        }

        return new String(out, "UTF-8");
    }


    /**
     * Decodes a base 64 or base 64 URL-safe encoded string. May contain
     * line separators. Any illegal characters are ignored.
     *
     * @param b64String The base 64 or base 64 URL-safe encoded string. May
     *                  be empty or {@code null}.
     *
     * @return The decoded byte array, empty if the input base 64 encoded
     *         string is empty, {@code null} or corrupted.
     */
    public static byte[] decode(final String b64String) throws Exception {

        // Check special case
        if (b64String == null || b64String.isEmpty()) {
            return new byte[0];
        }

        final byte[] srcBytes = b64String.getBytes("UTF-8");
        final int sLen = srcBytes.length;

        // Calculate output length assuming zero bytes are padding or separators
        final int maxOutputLen = sLen * 6 >> 3;

        // Allocate output array (may be too large)
        final byte[] dstBytes = new byte[maxOutputLen];

        // Process all input bytes
        int d = 0;
        for (int s = 0; s < srcBytes.length; ) {
            // Assemble three bytes into an int from four base 64
            // characters
            int i = 0;

            int j = 0;
            while (j < 4 && s < sLen) {
                // j only increased if a valid char was found
                final int c = decodeDigit(srcBytes[s++]);
                if (c >= 0) {
                    i |= c << (18 - j * 6);
                    j++;
                }
            }

            // j is now the number of valid digits decoded

            // Add output bytes
            if (j >= 2) {
                dstBytes[d++] = (byte) (i >> 16);
                if (j >= 3) {
                    dstBytes[d++] = (byte) (i >> 8);
                    if (j >= 4) {
                        dstBytes[d++] = (byte) i;
                    }
                }
            }
        }

        // d is now the number of output bytes written

        // Copy dstBytes to new array of proper size
        return Arrays.copyOf(dstBytes, d);
    }
}
