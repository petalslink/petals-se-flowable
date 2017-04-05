/**
 * Copyright (c) 2014-2017 Linagora
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
package org.ow2.petals.flowable.outgoing.cxf.transport;

import org.ow2.petals.component.framework.api.message.Exchange;

/**
 * A callback to transmit reply associated to a previous service invocation from the BPMN engine.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public interface AsyncCallback {

    /**
     * Process the reply received asynchronously from the NMR. This reply is associated to a previous request sent
     * through the BPMN engine.
     * 
     * @param asyncExchange
     *            The JBI message exchange containing the reply to return the BPMN engine,
     * @param cxfExchange
     *            The CXF exchange in which the reply must be transmitted to be returned to the BPMN engine through the
     *            CFX stack.
     */
    public void onMessage(final Exchange asyncExchange, final org.apache.cxf.message.Exchange cxfExchange);

    /**
     * Process a timeout about a previous request sent through the BPMN engine.
     * 
     * @param asyncExchange
     *            The expired JBI message exchange, initially sent through the BPMN engine.
     * @param cxfExchange
     *            The CXF exchange in which the expired reply must be transmitted to be returned to the BPMN engine
     *            through the CFX stack.
     */
    public void onExpiredMessage(final Exchange asyncExchange, final org.apache.cxf.message.Exchange cxfExchange);

}
