(let* ((jvm-constants:*constant-pool* (jvm-constants:make-constant-pool))
       (class-file
         (make-instance 'jvm-class-format:class-file
          :attributes #()
          :methods (list (make-instance 'jvm-class-format:method
                                        :attributes (list (make-instance 'jvm-class-format:code-attribute
                                                           :stack-size 1
                                                           :locals-size 1
                                                           :code #(42 176)
                                                           :exception-table #()
                                                           :attributes #()))
                                        :access-flags '(:public :static)
                                        :name (jvm-constants:constant-utf8 "identity")
                                        :descriptor (jvm-constants:constant-utf8 "(Ljava/lang/Object;)Ljava/lang/Object;")))
          :fields #()
          :interfaces #()
          :super-class (jvm-constants:class-named "java/lang/Object")
          :this-class  (jvm-constants:class-named "GoodMorningBeach")
          :access-flags '(:public)
          :constant-pool jvm-constants:*constant-pool*))
       (iolist (jvm-binary-output:render-value-of-type class-file
                                                       'jvm-class-format:class-file '())))
  (with-open-file (s "/tmp/GoodMorningBeach.class"
                     :direction :output
                     :element-type '(unsigned-byte 8))
    (jvm-binary-output:write-io-list iolist s)))

#|
Classfile /tmp/GoodMorningBeach.class
  Last modified 03/11/2020; size 155 bytes
  MD5 checksum cf4ca107809969f9c65ddff3f6bfa5bc
public class GoodMorningBeach
  minor version: 0
  major version: 52
  flags: ACC_PUBLIC
Constant pool:
  #1 = Class              #6              // GoodMorningBeach
  #2 = Class              #7              // java/lang/Object
  #3 = Utf8               identity
  #4 = Utf8               (Ljava/lang/Object;)Ljava/lang/Object;
  #5 = Utf8               Code
  #6 = Utf8               GoodMorningBeach
  #7 = Utf8               java/lang/Object
{
  public static java.lang.Object identity(java.lang.Object);
    descriptor: (Ljava/lang/Object;)Ljava/lang/Object;
    flags: ACC_PUBLIC, ACC_STATIC
    Code:
      stack=1, locals=1, args_size=1
         0: aload_0
         1: areturn
}
|#
