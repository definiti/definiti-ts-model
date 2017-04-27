export class DateWrapper {
    private inner: Date;

    constructor(inner: Date) {
        this.inner = inner;
    }

    get timestamp() {
        return new NumberWrapper(this.inner.getTime());
    }

    get day() {
        return new NumberWrapper(this.inner.getDate());
    }

    get month() {
        return new NumberWrapper(this.inner.getMonth());
    }

    toDate() {
        return this.inner;
    }

    equals(date: DateWrapper): boolean {
        return this.timestamp.equals(date.timestamp);
    }

    notEquals(date: DateWrapper): boolean {
        return this.timestamp.notEquals(date.timestamp);
    }

    upper(date: DateWrapper): boolean {
        return this.timestamp.upper(date.timestamp);
    }

    lower(date: DateWrapper): boolean {
        return this.timestamp.lower(date.timestamp);
    }

    upperOrEquals(date: DateWrapper): boolean {
        return this.timestamp.upperOrEquals(date.timestamp);
    }

    lowerOrEquals(date: DateWrapper): boolean {
        return this.timestamp.lowerOrEquals(date.timestamp);
    }
}