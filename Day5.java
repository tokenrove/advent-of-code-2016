
import java.security.MessageDigest;

class CountingBuffer {
    private static final int max_digits = 18;
    private byte[] bs;
    private long count;
    private int prefix_len, len, log10_count;

    public CountingBuffer(byte[] prefix, long count)
    {
        bs = new byte[prefix.length + max_digits];
        prefix_len = prefix.length;
        for (int i = 0; i < prefix_len; ++i)
            bs[i] = prefix[i];
        this.count = count;
        renderComplete();
    }

    private void renderComplete()
    {
        byte[] bytes = Long.toString(count).getBytes();
        log10_count = bytes.length - 1;
        for (int i = 0; i < bytes.length; ++i)
            bs[prefix_len+i] = bytes[i];
        this.len = bytes.length + prefix_len;
    }

    public void increment()
    {
        ++count;
        renderDigit(count, log10_count);
    }

    private void renderDigit(long n, int d)
    {
        if (d < 0) {
            System.arraycopy(bs, prefix_len, bs, prefix_len+1, ++log10_count);
            bs[prefix_len] = '1';
            ++this.len;
            return;
        }
        byte digit = (byte)('0' + n%10);
        bs[prefix_len+d] = digit;
        if ('0' == digit)
            renderDigit(n/10, d-1);
    }

    public byte[] bytes() { return bs; }
    public int len() { return len; }
}

class Day5 {
    public static void main(String args[])
        throws java.security.NoSuchAlgorithmException,
               java.security.DigestException
    {
        MessageDigest md = MessageDigest.getInstance("MD5");
        boolean easy_mode = args[0].equals("--initial");
        CountingBuffer cb = new CountingBuffer(args[easy_mode?1:0].getBytes(), 0);
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
