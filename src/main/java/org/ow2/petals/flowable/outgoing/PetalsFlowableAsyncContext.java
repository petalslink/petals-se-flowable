/**
 * Copyright (c) 2014-2026 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.outgoing;

import org.ow2.petals.flowable.outgoing.cxf.transport.AsyncCallback;
import org.ow2.petals.component.framework.process.async.AsyncContext;

public class PetalsFlowableAsyncContext extends AsyncContext {

    final AsyncCallback asyncCallback;

    final org.apache.cxf.message.Exchange cxfExchange;

    public PetalsFlowableAsyncContext(final org.apache.cxf.message.Exchange cxfExchange,
            final AsyncCallback asyncCallback) {
        this.asyncCallback = asyncCallback;
        this.cxfExchange = cxfExchange;
    }

    public AsyncCallback getAsyncCallback() {
        return this.asyncCallback;
    }

    public org.apache.cxf.message.Exchange getCxfExchange() {
        return this.cxfExchange;
    }

}
