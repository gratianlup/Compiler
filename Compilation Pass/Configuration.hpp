//! table with control options
//?     IntValue (id, value, name)
//?     FloatValue (id, value, name)
//?     BoolValue (id, enabled/disabled, name)

// config.GetValue<int>(id)
// config.GetValue<bool>("unroll-factor")




//! Pass enabled/disabled
//?      manually, automatically based on active configuration

// Debugging enabled?
//     - trace detail (enum None, Minimal, Complete)




//! Some default configuration
//! Active configuration, can be changed at runtime!!!
//?      - config based on optimization level
//?      - config based on architecture (target)
//?      - config based on 