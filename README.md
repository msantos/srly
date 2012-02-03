srly is a native interface to serial devices for Erlang, known to work
on Mac OS X, FreeBSD and Linux.

The C component of srly simply provides a wrapper to the system serial
interface. Data structures used as arguments to the native C functions
(such as struct termios) are provided as Erlang binaries. This allows
low level control of the serial device, at the expense of portability
(termios and structure alignment vary across platforms), performance
(due to portability, for some operations, the layout of the data structure
must be looked up at runtime) and safety (if the native system C interface
can be crashed by passing in arbitrary data as arguments).

For a safer, portable native interface, see cereal:

    https://github.com/joewilliams/cereal

(At some point, srly may be merged into cereal.)

## COMPILING

    make

## USAGE

_serctl_ is the interface to the native system C libraries and follows
the system C interface.

    serctl:open(Path) -> {ok, FD} | {error, posix()}

        Types   Path = list()
                FD = resource()

        Opens the serial device (e.g., "/dev/ttyUSB0").

    serctl:close(FD) -> ok | {error, posix()}

        Types   FD = resource()

        Closes the serial device. NOTE: If the process holding open the
        serial device exits, the file descriptor associated with the
        device will automatically be closed.

    serctl:read(FD, Size) -> {ok, Data} | {error, posix()}

        Types   FD = resource()
                Size = integer()
                Data = binary()

        Read from the serial device. Size is an unsigned long.

    serctl:write(FD, Data) -> ok | {error, posix()}

        Types   FD = resource()
                Data = binary()

        Write data to the serial device.


The low level interface follows the C library (see tcgetattr(3),
tcsetattr(3), cfsetispeed(3) and cfsetospeed(3) for details). For
convenience, atoms may be used in places where C has defined macros for
integers and Erlang records can be used as arguments instead of binaries.

To use Erlang records to represent the C struct termios (e.g., when
converting binaries using serctl:termios/1) include their definition:

    -include("serctl.hrl").


    serctl:tcgetattr(FD) -> {ok, Termios} | {error, posix()}

        Types   FD = resource()
                Termios = binary()

        Get the terminal attributes of the serial device. Returns the
        contents of the system struct termios as a binary.

    serctl:tcsetattr(FD, Action, Termios) -> ok | {error, posix()}

        Types   FD = resource()
                Action = integer() | Option | Options
                Options = [Option]
                Option = tcsanow | tcsadrain | tcsaflush
                Termios = binary() | #termios{}

        Sets the terminal attributes of the serial device.

        _Warning: the contents of Termios are passed directly to
        tcsettr(3). If the system tcsettr(3) does not check if the
        structure is valid, it may cause the library to crash, causing
        the Erlang VM to crash._

    serctl:cfsetispeed(Termios, Speed) -> {ok, Termios1} | {error, posix()}

        Types   Termios = binary() | #termios{}
                Speed = integer() | atom()
                Termios1 = binary()

        Set the input speed of the serial device. See the warning for
        tcsetattr/2.

    serctl:cfsetospeed(Termios, Speed) -> {ok, Termios1} | {error, posix()}

        Types   Termios = binary() | #termios{}
                Speed = integer() | atom()
                Termios1 = binary()

        Set the input speed of the serial device. See the warning for
        tcsetattr/2.

    serctl:getfd(FD) -> integer()

        Types   FD = resource()

        Returns the file descriptor associated with the NIF resource. The
        file descriptor can be used with erlang:open_port/2.

    serctl:constant() -> Constants
    serctl:constant(Attr) -> integer()

        Types   Constants = [{Attr, integer()}]
                Attr = tcsaflush | tcsadrain | tcsanow | tcioflush | tcoflush | tciflush |
                    tcion | tcioff | tcoon | tcooff | iexten | tostop | noflsh | echonl |
                    echoke | echok | echoe | echo | icanon | isig | crtscts | b1152000 |
                    b1000000 | b921600 | b576000 | b500000 | b460800 | b230400 | b115200 |
                    b57600 | clocal | hupcl | parodd | parenb | cread | cstopb | cs8 | cs7 | cs6 |
                    cs5 | csize | b38400 | b19200 | b9600 | b4800 | b2400 | b1800 | b1200 | b600 |
                    b300 | b200 | b150 | b134 | b110 | b75 | b50 | b0 | ofdel | ofill | onlret |
                    onocr | ocrnl | onlcr | olcuc | opost | iutf8 | imaxbel | ixoff | ixany |
                    ixon | iuclc | icrnl | igncr | inlcr | istrip | inpck | parmrk | ignpar |
                    brkint | ignbrk | veol2 | vlnext | vwerase | vdiscard | vreprint | veol |
                    vsusp | vstop | vstart | vswtc | vmin | vtime | veof | vkill | verase | vquit |
                    vintr | nccs

        Map of atoms reprsenting terminal attribute constants to
        integers. Varies across platforms.


serctl has a higher level interface for manipulating the C data structures
that takes care of portability. The structures are represented as Erlang
records. These functions do not have side effects and only retrieve or
set values within the termios structure. To change the serial device,
the attributes must be written out using serctl:tcsetattr/3.

    serctl:flow(Termios) -> true | false
    serctl:flow(Termios, Bool) -> #termios{}

        Types   Termios = binary() | #termios{}
                Bool = true | false

        flow/1 indicates whether flow control is enabled in a serial
        device's terminal attributes. flow/2 returns a termios structure
        that can be used for setting a serial device's flow control.

    serctl:mode(Mode) -> #termios{}

        Types   Mode = raw

        Returns an Erlang termios record with attributes that can be
        used to put the serial device into raw mode.

    serctl:getflag(Termios, Flag, Opt) -> true | false

        Types   Termios = binary() | #termios{}
                Flag = cflag | lflag | iflag | oflag
                Opt = atom()

        Returns whether a flag is enabled. Opt is one of the atoms
        returned using serctl:constant/0.

    serctl:setflag(Termios, Opt) -> #termios{}

        Types   Termios = #termios{}
                Opt = [Param]
                Param = {Flag, [Val]}
                Flag = cflag | lflag | iflag | oflag
                Val = {atom(), Bool}
                Bool = true | false

        Returns an Erlang termios record which can be used for setting
        the attributes of a serial device. For example, to create
        attributes that can be used to enable hardware flow control on
        a serial device:

            {ok, FD} = serctl:open("/dev/ttyUSB0"),
            {ok, Termios} = serctl:tcgetattr(FD),
            Termios1 = serctl:setflag(Termios, [{cflag, [{crtscts, true}]}]),
            ok = serctl:tcsetattr(FD, tcsanow, Termios1).

    serctl:ispeed(Termios) -> integer()
    serctl:ispeed(Termios, Speed) -> #termios{}
    serctl:ospeed(Termios) -> integer()
    serctl:ospeed(Termios, Speed) -> #termios{}

        Types   Termios = #termios{}
                Speed = integer()

        ispeed/1 and ospeed/1 return the input and output speed of the
        serial device. Note the speed returned is the constant defined
        for the system and may differ between platforms.

        ispeed/2 and ospeed/2 return an Erlang termios record that can be
        used for setting the input and output speed of the serial device.

    serctl:baud(Speed) -> integer()

        Types   Speed = 115200 | 19200 | 9600 | ...

        Convenience function returning the constant defined for the baud
        rate for the platform.

    serctl:termios(Termios) -> #termios{} | binary()

        Types   Termios = #termios{} | binary()

        Converts between the C struct termios and the Erlang record
        representation.


## TODO

* document srly

* test if the system C interface can actually be crashed!
