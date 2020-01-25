type handler = unit => unit;
type unregisterF = handler;

let staleHandlers = ref([]);
let addStaleTreeHandler = (handler: handler) => {
  staleHandlers := [handler, ...staleHandlers^];
  () => {
    staleHandlers := List.filter(f => f === handler, staleHandlers^);
  };
};
let callStaleHandlers = () => List.iter(f => f(), staleHandlers^);

