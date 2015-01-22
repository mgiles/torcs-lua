**Work in progress**

This is a sample driver for TORCS that allows the driver logic to be implemented in Lua instead of C++. The bindings are currently in progress and somewhat limited, but will continue to be filled in.

## Compatibility

Tested on Debian with TORCS 1.3.6 and Lua 5.1

## Basic setup instructions

1. Download, build, and install TORCS (instructions [here](http://torcs.sourceforge.net/index.php?name=Sections&op=viewarticle&artid=3))
2. Clone this repository into the TORCS drivers directory (ex. into `torcs-1.3.6/src/drivers`)
3. `make && make install`
4. Now in TORCS when you're configuring a race there will be a `lua_sample` driver available.

For building TORCS bots in general, you need to set the following environment variables.

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
export TORCS_BASE=/path/to/torcs/base
export MAKE_DEFAULT=$TORCS_BASE/Make-default.mk
```

You can find more instructions for getting TORCS itself set up in berniw's
[TORCS Robot Tutorial](http://www.berniw.org/tutorials/robot/tutorial.html).
The robot tutorial is a bit out of date, so you may need to deviate from it in places. Try the
[torcs-users](http://sourceforge.net/p/torcs/mailman/torcs-users/) mailing list if you run into problems.

## Further setup

In the `Makefile`, make sure the `LIBS` and `COMPILFLAGS` variables use the correct paths for your installation of Lua.
The existing values are for the default install locations for Lua 5.1 on Debian.

The path to the main Lua script (`drive.lua` by default) is set in `lua_bridge.cpp` in the `initLua` function. It relies on the same `TORCS_BASE` environment variable described above. If you rename or move the Lua script, you'll need to make sure this function can still find it.

## Bindings

You can find the set of currently implemented bindings in `dispatch.h`. Look at the `fields_*` arrays in that file to find the field names that can be accessed from Lua for the given type. The conventions that the bindings follow are explained in `dispatch.cpp`, where the actual bindings are defined.

The following Lua code is included in the sample, under `lua/drive.lua`, and demonstrates how field access works:

```lua
function drive (car)
  return { gear = getGear(car), accelCmd = 0.3 }
end

function getGear (car)
  local gear = car.priv.gear
  local max = car.priv.gearNb

  if gear <= 0 then
    return 1
  elseif gear < max and car.priv.enginerpm >= car.priv.enginerpmRedLine then
    return gear + 1
  else
    return gear
  end
end
```

The fields correspond to the TORCS struct definitions. The `car` value that is passed into `drive` is a `tCarElt` value, which has a field `priv` (returning a `tPrivCar` value). `tPrivCar` in turn has fields like `gear` and `gearNb`. For all of these fields you can see what function ends up being called by looking at the `fields_*` tables in `dispatch.h`.

The `drive` function must return a Lua table with the vehicle control fields in it. The available fields are `steer`, `accelCmd`, `brakeCmd`, `clutchCmd`, `gear`, `raceCmd`, and `lightCmd`. All of the fields have a default value of 0 if you don't set them. You can see how these fields are passed back to TORCS in the `tl_drive` function in `lua_bridge.cpp`.
