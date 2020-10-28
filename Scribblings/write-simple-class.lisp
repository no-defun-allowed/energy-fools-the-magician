(let* ((jvm-constants:*constant-pool* (jvm-constants:make-constant-pool))
       (class-file
         (make-instance 'jvm-opcodes:class-file
                        :attributes #()
                        :methods #()
                        :fields #()
                        :interfaces #()
                        :super-class (jvm-constants:class-named "java/lang/Object")
                        :this-class  (jvm-constants:class-named "GoodMorningBeach")
                        :access-flags '(:public)
                        :constant-pool jvm-constants:*constant-pool*))
       (iolist (jvm-binary-output:render-value-of-type class-file
                                                       'jvm-opcodes:class-file '())))
  (with-open-file (s "/tmp/GoodMorningBeach.class"
                     :direction :output
                     :element-type '(unsigned-byte 8))
    (jvm-binary-output:write-io-list iolist s)))

#|
Classfile /tmp/GoodMorningBeach.class
  Last modified 28/10/2020; size 68 bytes
  MD5 checksum a659aa5078deb4aa3389d936388667f8
public class GoodMorningBeach
  minor version: 0
  major version: 52
  flags: ACC_PUBLIC
Constant pool:
  #1 = Class              #3              // GoodMorningBeach
  #2 = Class              #4              // java/lang/Object
  #3 = Utf8               GoodMorningBeach
  #4 = Utf8               java/lang/Object
{
}
|#
