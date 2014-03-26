using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace RobossInterface {
    
    class ErlComm {

        public ErlComm() {
            var st = Console.OpenStandardInput();
            var sr = new StreamReader(st, Encoding.GetEncoding("iso8859-1"));
            Console.SetIn(sr);

            var sto = Console.OpenStandardOutput();
            var sro = new StreamWriter(sto, Encoding.GetEncoding("iso8859-1")) { AutoFlush = true };
            Console.SetOut(sro);
        }

        public int ReadCmd(byte[] buf) {
            if (ReadExact(buf, 2) != 2)
                return (-1);
            var len = (buf[0] << 8) | buf[1];

            return ReadExact(buf, len);
        }

        public int WriteCmd(byte[] buf, int len) {
            var li = new byte[1];

            li[0] = (byte)((len >> 8) & 0xff);
            WriteExact(li, 1);

            li[0] = (byte)(len & 0xff);
            WriteExact(li, 1);

            return WriteExact(buf, len);
        }

        private int ReadExact(byte[] buf, int len) {
            int got = 0;

            do {
                long tmp = 65;
                if ((tmp = Console.Read()) < 0) {
                    buf[got] = (byte)tmp;
                    return got;
                }
                
                buf[got] = (byte)tmp;
                got++;
            } while (got < len);

            return (len);
        }

        private int WriteExact(byte[] buf, int len) {
            using (Stream console = Console.OpenStandardOutput()) {
                console.Write(buf, 0, len);
                return (len);
            }
        }
    }
}
