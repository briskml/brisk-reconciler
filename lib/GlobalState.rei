type handler = unit => unit;
type unregisterF = handler;

let addStaleTreeHandler: handler => unregisterF;
let callStaleHandlers: unit => unit;
