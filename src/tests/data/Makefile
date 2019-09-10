.PHONY: all arm-all thm-all clean

ARM_BINS=$(patsubst %.S,%.bin,$(wildcard *arm*.S))
THM_BINS=$(patsubst %.S,%.bin,$(wildcard *thm*.S))

all: arm-all thm-all

arm-all: $(ARM_BINS)

thm-all: $(THM_BINS)

emutest_arm%.bin: emutest_arm%.S
	arm-none-eabi-as -mcpu=arm7tdmi $<
	data=$$(arm-none-eabi-objdump -h a.out | grep ".text" | awk '{ print $$3, $$6; }'); \
		 len=$$(python -c "print(0x$$(echo $${data} | cut -f 1 -d ' '))"); \
		 offset=$$(python -c "print(0x$$(echo $${data} | cut -f 2 -d ' '))"); \
		 echo len: $${len}; \
		 echo offset: $${offset}; \
		 dd skip=$${offset} count=$${len} if=a.out of=$@ bs=1
	printf '0x10addee7' | xxd -r >> $@
	-rm a.out

emutest_thm%.bin: emutest_thm%.S
	arm-none-eabi-as -mcpu=arm7tdmi $<
	data=$$(arm-none-eabi-objdump -h a.out | grep ".text" | awk '{ print $$3, $$6; }'); \
		 len=$$(python -c "print(0x$$(echo $${data} | cut -f 1 -d ' '))"); \
		 offset=$$(python -c "print(0x$$(echo $${data} | cut -f 2 -d ' '))"); \
		 echo len: $${len}; \
		 echo offset: $${offset}; \
		 dd skip=$${offset} count=$${len} if=a.out of=$@ bs=1
	printf '0x00e8' | xxd -r >> $@
	-rm a.out

clean:
	rm -f *.bin
	rm -f a.out
