"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.protect = protect;
exports.add = add;
exports.choose = choose;
exports.filter = filter;
exports.map = map;
exports.merge = merge;
exports.pairwise = pairwise;
exports.partition = partition;
exports.scan = scan;
exports.split = split;
exports.subscribe = subscribe;
exports.Observer = void 0;

var _Option = require("./Option");

class Observer {
  constructor(onNext, onError, onCompleted) {
    this.OnNext = onNext;

    this.OnError = onError || (e => {
      return;
    });

    this.OnCompleted = onCompleted || (() => {
      return;
    });
  }

}

exports.Observer = Observer;

class Observable {
  constructor(subscribe) {
    this.Subscribe = subscribe;
  }

}

function protect(f, succeed, fail) {
  try {
    return succeed(f());
  } catch (e) {
    fail(e);
  }
}

function add(callback, source) {
  source.Subscribe(new Observer(callback));
}

function choose(chooser, source) {
  return new Observable(observer => source.Subscribe(new Observer(t => protect(() => chooser(t), u => {
    if (u != null) {
      observer.OnNext((0, _Option.value)(u));
    }
  }, observer.OnError), observer.OnError, observer.OnCompleted)));
}

function filter(predicate, source) {
  return choose(x => predicate(x) ? x : null, source);
}

function map(mapping, source) {
  return new Observable(observer => source.Subscribe(new Observer(t => {
    protect(() => mapping(t), observer.OnNext, observer.OnError);
  }, observer.OnError, observer.OnCompleted)));
}

function merge(source1, source2) {
  return new Observable(observer => {
    let stopped = false;
    let completed1 = false;
    let completed2 = false;
    const h1 = source1.Subscribe(new Observer(v => {
      if (!stopped) {
        observer.OnNext(v);
      }
    }, e => {
      if (!stopped) {
        stopped = true;
        observer.OnError(e);
      }
    }, () => {
      if (!stopped) {
        completed1 = true;

        if (completed2) {
          stopped = true;
          observer.OnCompleted();
        }
      }
    }));
    const h2 = source2.Subscribe(new Observer(v => {
      if (!stopped) {
        observer.OnNext(v);
      }
    }, e => {
      if (!stopped) {
        stopped = true;
        observer.OnError(e);
      }
    }, () => {
      if (!stopped) {
        completed2 = true;

        if (completed1) {
          stopped = true;
          observer.OnCompleted();
        }
      }
    }));
    return {
      Dispose() {
        h1.Dispose();
        h2.Dispose();
      }

    };
  });
}

function pairwise(source) {
  return new Observable(observer => {
    let last = null;
    return source.Subscribe(new Observer(next => {
      if (last != null) {
        observer.OnNext([last, next]);
      }

      last = next;
    }, observer.OnError, observer.OnCompleted));
  });
}

function partition(predicate, source) {
  return [filter(predicate, source), filter(x => !predicate(x), source)];
}

function scan(collector, state, source) {
  return new Observable(observer => {
    return source.Subscribe(new Observer(t => {
      protect(() => collector(state, t), u => {
        state = u;
        observer.OnNext(u);
      }, observer.OnError);
    }, observer.OnError, observer.OnCompleted));
  });
}

function split(splitter, source) {
  return [choose(v => (0, _Option.tryValueIfChoice1)(splitter(v)), source), choose(v => (0, _Option.tryValueIfChoice2)(splitter(v)), source)];
}

function subscribe(callback, source) {
  return source.Subscribe(new Observer(callback));
}