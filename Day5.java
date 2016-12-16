
import java.security.MessageDigest;

class CountingBuffer {
    private static final int max_digits = 18;
    private byte[] bs;
    private int prefix_len, len, log10_count;

    public CountingBuffer(byte[] prefix)
    {
        bs = new byte[prefix.length + max_digits];
        prefix_len = prefix.length;
        System.arraycopy(prefix, 0, bs, 0, prefix_len);
        bs[prefix_len] = '0';
        log10_count = 0;
    }

    public void increment()
    {
        renderDigit(log10_count);
    }

    private void renderDigit(int d)
    {
        if (d < 0) {
            System.arraycopy(bs, prefix_len, bs, prefix_len+1, ++log10_count);
            assert(log10_count < max_digits);
            bs[prefix_len] = '1';
            return;
        }
        byte digit = ++bs[prefix_len+d];
        if (':' == digit) {     // sry ebcdic
            bs[prefix_len+d] = '0';
            renderDigit(d-1);
        }
    }

    public byte[] bytes() { return bs; }
    public int len() { return prefix_len + log10_count + 1; }
}

class Day5 {
    public static void main(String args[])
        throws java.security.NoSuchAlgorithmException,
               java.security.DigestException
    {
        MessageDigest md = MessageDigest.getInstance("MD5");
        boolean easy_mode = args[0].equals("--initial");
        CountingBuffer cb = new CountingBuffer(args[easy_mode?1:0].getBytes());
        byte[] out = new byte[16], pw = new byte[8];

        int pwchars = 0;
        while (pwchars != 0xff) {
            md.reset();
            md.update(cb.bytes(), 0, cb.len());
            md.digest(out, 0, 16);
            if (0 == out[0] &&
                0 == out[1] &&
                0 == (out[2]&0xf0)) {
                if (easy_mode) {
                    pw[Integer.bitCount(pwchars)] = (byte)(out[2]&0xf);
                    pwchars = 1|(pwchars<<1);
                } else {
                    int pos = out[2]&0xf;
                    int c = (out[3]>>>4)&0xf;
                    if (pos < 8 && 0 == (pwchars&(1<<pos))) {
                        pw[pos] = (byte)c;
                        pwchars |= 1<<pos;
                    }
                }
            }
            cb.increment();
        }
        for (int i = 0; i < 8; ++i)
            System.out.print(Integer.toHexString(pw[i]));
        System.out.println();
    }
}
