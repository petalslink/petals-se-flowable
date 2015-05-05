/**
 * Copyright (c) 2014-2015 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.outgoing;

import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;

import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.component.framework.process.async.AsyncContext;

/**
 * The Petals context.
 * 
 * @author Christophe DENEUX - Linagora
 */
public interface PetalsContext {

    /**
     * @return The logger of the JBI component
     */
    public Logger getLogger();

    /**
     * Send asynchronously an exchange. The response will be provided as an
     * asynchronous exchange
     * 
     * @param exchange
     *            The exchange to send
     * @param asyncContext
     *            The asynchronous context, provided on the response
     * @throws MessagingException
     */
    public void sendAsync(final Exchange exchange, AsyncContext asyncContext)
            throws MessagingException;

    /**
     * Create an exchange implementation matching the given pattern.
     * 
     * @param mep
     *            a specific MEP pattern
     * @return an exchange implementation matching the given pattern
     * @throws MessagingException
     *             The message exchange can't be created or no pattern specified in extensions
     * @throws PEtALSCDKException
     *             The current Transaction has failed to return its status
     */
    public Exchange createExchange(final MEPPatternConstants mep) throws MessagingException,
            PEtALSCDKException;
}
