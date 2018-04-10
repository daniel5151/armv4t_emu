.PHONY: arm-all clean

ARM_BINS=$(patsubst %.S,%.bin,$(wildcard *arm*.S))

arm-all: $(ARM_BINS)

emutest_arm%.bin: emutest_arm%.S
	arm-none-eabi-as $<
	data=$$(arm-none-eabi-objdump -h a.out | grep ".text" | awk '{ print $$3, $$6; }'); \
		 len=$$(python -c "print(0x$$(echo $${data} | cut -f 1 -d ' '))"); \
		 offset=$$(python -c "print(0x$$(echo $${data} | cut -f 2 -d ' '))"); \
		 echo len: $${len}; \
		 echo offset: $${offset}; \
		 dd skip=$${offset} count=$${len} if=a.out of=$@ bs=1
	printf '0x10addeef' | xxd -r >> $@
	-rm a.out

clean:
	rm *.bin
