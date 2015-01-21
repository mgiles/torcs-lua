#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <car.h>

// full userdata wrapping the car state
typedef struct {
  tCarElt *car;
} tTorcsState;

// Field lookup function
typedef int (*tFieldGetter) (lua_State *L, tCarElt *car);

// Field dispatch entry
typedef struct tFieldEntry {
  const char *name;
  tFieldGetter getter;
} tFieldEntry;

// All top-level, "torcs" namespace functions
extern const struct luaL_Reg torcs_functions[];

int tl_newTorcsState(lua_State *L);

// All field access methods on tTorcsState userdata
extern const struct tFieldEntry carFields[];

/**
   Params on stack:
   1 - tTorcsState
   2 - string (field name)

   Result on stack:
   1 - Result of field call (type depends on field)
 */
int tl_lookupField(lua_State *L);

// All getters take no arguments on the stack, and leave one result

// Hand-written getters

int tl_steerLock(lua_State *L, tCarElt *car);

// Generated getters

int tl_driverIndex(lua_State *L, tCarElt *car);
int tl_gear(lua_State *L, tCarElt *car);
int tl_fuel(lua_State *L, tCarElt *car);
int tl_enginerpm(lua_State *L, tCarElt *car);
int tl_enginerpmRedLine(lua_State *L, tCarElt *car);
int tl_enginerpmMax(lua_State *L, tCarElt *car);
int tl_enginerpmMaxTq(lua_State *L, tCarElt *car);
int tl_enginerpmMaxPw(lua_State *L, tCarElt *car);
int tl_engineMaxTq(lua_State *L, tCarElt *car);
int tl_engineMaxPw(lua_State *L, tCarElt *car);
int tl_gearNb(lua_State *L, tCarElt *car);
int tl_gearOffset(lua_State *L, tCarElt *car);
int tl_dammage(lua_State *L, tCarElt *car);
int tl_debug(lua_State *L, tCarElt *car);
