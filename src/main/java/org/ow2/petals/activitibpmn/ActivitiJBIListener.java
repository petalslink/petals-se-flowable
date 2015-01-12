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
package org.ow2.petals.activitibpmn;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import org.ow2.petals.activitibpmn.operation.ActivitiOperation;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.component.framework.listener.AbstractJBIListener;


/**
 * Listens to messages incoming from inside Petals.
 * <p>
 * This class is in charge of processing messages coming from a Petals consumer. These messages can be requests (in
 * messages) or acknowledgments (ACK).
 * </p>
 * <p>
 * Depending on the invoked operation, the message exchange pattern (MEP) and the component's logic, this class may
 * build and send a response.
 * </p>
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 */
public class ActivitiJBIListener extends AbstractJBIListener {
	
    /**
     * A prefix to use for logged messages.
     * <p>
     * This prefix is updated for every processed message.<br />
     * It only contains the exchange ID.
     * </p>
     */
    private String logHint;

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean onJBIMessage(final Exchange exchange) {
 
        final Logger logger = this.getLogger();
        logger.fine("Start ActivitiJBIListener.onJBIMessage()");
        try {
            if (exchange.isActiveStatus()) {
                this.logHint = "Exchange " + exchange.getExchangeId();

                // Provider role
                if (exchange.isProviderRole()) {
                    try {
                        // Get the InMessage
                        final NormalizedMessage normalizedMessage = exchange.getInMessage();
                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine("normalizedMessage = " + normalizedMessage.toString());
                        }

                        // Validate Message pattern
                        if (!exchange.isInOutPattern()) {
                            // TODO: Add a unit test
                            if (logger.isLoggable(Level.WARNING)) {
                                logger.warning(this.logHint
                                        + " encountered a problem. The exchange pattern must be IN/OUT !");
                            }
                            throw new MessagingException("The exchange pattern must be IN/OUT !");
                        }

                        // TODO Validate Message

                        // Get the eptName and Operation
                        final String eptName = exchange.getEndpointName();
                        // TODO: Caution: the namespace must be used, the local part is not sufficient to identify an
                        // operation
                        final String operationName = exchange.getOperationName();
                        // Set eptAndoperation
                        final EptAndOperation eptAndOperation = new EptAndOperation(eptName, operationName);

                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine(logHint + " was received and is started to be processed.");
                            logger.fine("interfaceName = " + exchange.getInterfaceName());
                            logger.fine("Service       = " + exchange.getService());
                            logger.fine("EndpointName  = " + eptName);
                            logger.fine("OperationName = " + operationName);
                            logger.fine("Pattern " + exchange.getPattern());
                        }

                        // Get the Activiti Operation from the Map eptOperationToActivitiOperation
                        // @see org.ow2.petals.activitibpmn.ActivitiSE
                        final ActivitiOperation activitiOperation = ((ActivitiSE) this.component)
                                .getActivitiOperations(eptAndOperation);
                        if (activitiOperation == null) {
                            // TODO: Create a unit test
                            throw new MessagingException("No BPMN operation found matching the exchange");
                        }

                        activitiOperation.execute(exchange);

                    } catch (final MessagingException e) {
                        logger.log(Level.SEVERE, "Exchange " + exchange.getExchangeId() + " encountered a problem.", e);
                        // Technical error, it would be set as a Fault by the CDK
                        exchange.setError(e);
                    }
                } else {
                    // TODO: to do
                }
            } else if (exchange.isErrorStatus()) {
                logger.warning("Exchange " + exchange.getExchangeId() + " received with a status 'ERROR'. Skipped !");
            }

            // True to let the CDK close the exchange.
            // False to explicitly return the exchange.
            return true;
        } finally {
            logger.fine("End ActivitiJBIListener.onJBIMessage()");
        }
    }
}