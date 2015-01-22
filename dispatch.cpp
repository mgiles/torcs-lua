#include "dispatch.h"

/**
   Conventions for bindings:

   - The macros rely on particular naming conventions and will break if those aren't followed.
   - For every TORCS struct there's a corresponding wrapper that gets passed to Lua
     - Ex. tl_CarElt wraps tCarElt
   - Each wrapper has a metatable to dispatch field lookup
     - Ex. tl_CarElt's metatable is registered as "torcs.CarElt"
   - Each TORCS struct has a dispatch table, defined in dispatch2.h
     - Ex. tl_CarElt's dispatch table is `fields_CarElt`
   - Getters receive the lua_State and a pointer to their TORCS structure. They must
     return their result on the Lua stack.
     - Ex. getter(CarElt, index, integer) -> int f_CarElt_index(lua_State *L, tCarElt *car)
       and the generated function calls `lua_pushinteger` to return `car->index`
   - Struct getters wrap the result and set its metatable before returning it
     - Ex. structGetter(CarElt, InitCar, info) creates a tl_InitCar userdata on the stack
       and sets its metatable to the registered "torcs.InitCar" table
 */

/** Binding generator macros**/

// Create the dispatch function for the given struct type (ex. CarElt)
#define dispatch(TYPE) \
  int dispatch_##TYPE(lua_State *L) { \
    tl_##TYPE *wrapper = (tl_##TYPE *) luaL_checkudata(L, 1, "torcs." #TYPE); \
    luaL_argcheck(L, wrapper != NULL, 1, "Expected tl_" #TYPE); \
    \
    t##TYPE *wrapped = wrapper->wrapped; \
    const char *field = luaL_checkstring(L, 2); \
    \
    getter_##TYPE f = NULL; \
    int i = 0; \
    while (fields_##TYPE[i].name != NULL && strcmp(fields_##TYPE[i].name, field) != 0) { \
      i++; \
    } \
    \
    f = fields_##TYPE[i].getter; \
    if (f != NULL) { \
      return f(L, wrapped); \
    } else { \
      missingFieldError(L, field); \
      return -1; \
    } \
  }

// Create a getter for the given type, field, and corresponding Lua type
// Ex. getter(CarElt, index, integer) calls `lua_pushinteger`, passing car->index
// where car is a (tCarElt *)
#define getter(TYPE, FIELD, LUATYPE) \
  int f_ ## TYPE ## _ ## FIELD(lua_State *L, t ## TYPE *wrapped) { \
  lua_push ## LUATYPE(L, wrapped->FIELD); \
  return 1; \
  }

// Create a getter for a nested struct. TYPE is the containing struct, FIELD_TYPE
// is the contained struct, and FIELD is the field name.
// Ex. TYPE=CarElt, FIELD_TYPE=InitCar, FIELD=info
#define structGetter(TYPE, FIELD_TYPE, FIELD) \
  int f_##TYPE##_##FIELD(lua_State *L, t##TYPE *s) { \
    tl_##FIELD_TYPE *wrapper = (tl_##FIELD_TYPE *) lua_newuserdata(L, sizeof(tl_##FIELD_TYPE)); \
    wrapper->wrapped = &(s->FIELD); \
    \
    luaL_getmetatable(L, "torcs." #FIELD_TYPE); \
    lua_setmetatable(L, -2); \
    return 1; \
  }

/** Generic helpers **/

static void missingFieldError(lua_State *L, const char *field) {
  const char *desc = "No such field: ";
  char error[strlen(desc) + strlen(field) + 1];
  sprintf(error, "%s%s", desc, field);

  lua_pushstring(L, error);
  lua_error(L);
}


/** tCarElt **/

dispatch(CarElt)

getter(CarElt, index, integer)

structGetter(CarElt, InitCar, info)
structGetter(CarElt, PrivCar, priv)

/** tInitCar **/

dispatch(InitCar)

getter(InitCar, name, string)
getter(InitCar, teamname, string)
getter(InitCar, carName, string)
getter(InitCar, category, string)
getter(InitCar, raceNumber, integer)
getter(InitCar, startRank, integer)
getter(InitCar, driverType, integer)
getter(InitCar, skillLevel, integer)

//int f_InitCar_iconColor(lua_State *L, tInitCar *info);
//int f_InitCar_dimension(lua_State *L, tInitCar *info);
//int f_InitCar_drvPos(lua_State *L, tInitCar *info);
//int f_InitCar_bonnetPos(lua_State *L, tInitCar *info);

getter(InitCar, tank, number)
getter(InitCar, steerLock, number)

//int f_InitCar_statGC(lua_State *L, tInitCar *info);
//int f_InitCar_wheel(lua_State *L, tInitCar *info);
//int f_InitCar_visualAttr(lua_State *L, tInitCar *info);


/** tPrivCar **/

dispatch(PrivCar)

getter(PrivCar, driverIndex, integer)
getter(PrivCar, modName, string)
//int f_PrivCar_wheel(lua_State *L, tPrivCar *priv);
//int f_PrivCar_corner(lua_State *L, tPrivCar *priv);
getter(PrivCar, gear, integer)
getter(PrivCar, fuel, number)
getter(PrivCar, enginerpm, number)
getter(PrivCar, enginerpmRedLine, number)
getter(PrivCar, enginerpmMax, number)
getter(PrivCar, enginerpmMaxTq, number)
getter(PrivCar, enginerpmMaxPw, number)
getter(PrivCar, engineMaxTq, number)
getter(PrivCar, engineMaxPw, number)
//int f_PrivCar_gearRatio(lua_State *L, tPrivCar *priv);
getter(PrivCar, gearNb, integer)
getter(PrivCar, gearOffset, integer)
//int f_PrivCar_skid(lua_State *L, tPrivCar *priv);
//int f_PrivCar_reaction(lua_State *L, tPrivCar *priv);
getter(PrivCar, collision, integer)
getter(PrivCar, simcollision, integer)
getter(PrivCar, smoke, number)
//int f_PrivCar_normal(lua_State *L, tPrivCar *priv);
//int f_PrivCar_collpos(lua_State *L, tPrivCar *priv);
getter(PrivCar, dammage, integer)
getter(PrivCar, debug, integer)
//int f_PrivCar_collision_state(lua_State *L, tPrivCar *priv);
