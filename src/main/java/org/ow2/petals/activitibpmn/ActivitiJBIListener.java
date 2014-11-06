/**
 * Copyright (c) 2014 Linagora
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

import java.io.ByteArrayInputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.ow2.petals.activitibpmn.operation.ActivitiOperation;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.component.framework.listener.AbstractJBIListener;
import org.w3c.dom.Document;

import com.ebmwebsourcing.easycommons.xml.Transformers;


/**
 * Listens to messages incoming from inside Petals.
 * <p>
 * This class is in charge of processing messages coming from a Petals
 * consumer. These messages can be requests (in messages) or acknowledgments (ACK).
 * </p>
 * <p>
 * Depending on the invoked operation, the message exchange pattern (MEP) and
 * the component's logic, this class may build and send a response.
 * </p>
 * 
 * @author bescudie
 */
public class ActivitiJBIListener extends AbstractJBIListener {

    /**
     * The IdentityService of the ProcessEngine.
     * <p>
     * This IdentityService is initialized once <br />
     * </p>
     */
	private IdentityService identityService; 

    /**
     * The RuntimeService of the ProcessEngine.
     * <p>
     * This RuntimeService is initialized once <br />
     * </p>
     */
	private RuntimeService runTimeService; 
    /**
     * The TaskService of the ProcessEngine.
     * <p>
     * This TaskService is initialized once <br />
     * </p>
     */
	private TaskService taskService; 

	/**
     * The FormService of the ProcessEngine.
     * <p>
     * This FormService is initialized once <br />
     * </p>
     */
//	private FormService formService; 
	
	
    /**
     * The TaskService of the ProcessEngine.
     * <p>
     * This TaskService is initialized once <br />
     * </p>
     */
//	private TaskService taskService; 

//	private RepositoryService repositoryService; 
	
	
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
    public void init() {
        if (this.component instanceof ActivitiSE) {
            final ActivitiSE activitiComp = (ActivitiSE) this.component;

            // Initialize the identityService if necessary
            if (this.identityService == null) {
                this.identityService = activitiComp.getProcessEngine().getIdentityService();
            }
            // Initialize the runTimeService if necessary
            if (this.runTimeService == null) {
                this.runTimeService = activitiComp.getProcessEngine().getRuntimeService();
            }
            // Initialize the taskService if necessary
            if (this.taskService == null) {
                this.taskService = activitiComp.getProcessEngine().getTaskService();
            }
            // Initialize the repositoryService if necessary
            // if ( this.repositoryService == null) { this.repositoryService =
            // activitiComp.getProcessEngine().getRepositoryService(); }
            // Initialize the formService if necessary
            // if ( this.formService == null) { this.formService = activitiComp.getProcessEngine().getFormService(); }
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean onJBIMessage(final Exchange exchange) {
 
        final Logger logger = getLogger();
        logger.fine("Start ActivitiJBIListener.onJBIMessage()");
        try {
            Exception finalException = null;

            if (exchange.isActiveStatus()) {
                this.logHint = "Exchange " + exchange.getExchangeId();
                try {
                    // Get the InMessage
                    final NormalizedMessage normalizedMessage = exchange.getInMessage();
                    if (logger.isLoggable(Level.FINE))
                        logger.fine("normalizedMessage = " + normalizedMessage.toString());

                    // Provider role
                    if (exchange.isProviderRole()) {

                        // Validate Message pattern
                        if (!exchange.isInOutPattern()) {
                            if (logger.isLoggable(Level.WARNING)) {
                                logger.warning(this.logHint
                                        + " encountered a problem. The exchange pattern must be IN/OUT !");
                            }
                            throw new MessagingException("The exchange pattern must be IN/OUT !");
                        }

                        // TODO Validate Message

                        // Get the eptName and Operation
                        String eptName = exchange.getEndpointName();
                        String operationName = exchange.getOperationName();
                        // Set eptAndoperation
                        EptAndOperation eptAndOperation = new EptAndOperation(eptName, operationName);

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

                        // Get the exchange data
                        final Document inMsgWsdl = exchange.getInMessageContentAsDocument();
                        final DOMSource domSource = new DOMSource(inMsgWsdl);
                        final StringWriter writer = new StringWriter();
                        final StreamResult result = new StreamResult(writer);
                        final Transformer transformer = Transformers.takeTransformer();
                        try {
                            transformer.transform(domSource, result);
                        } finally {
                            Transformers.releaseTransformer(transformer);
                        }

                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine("*** inMsgWsdl = " + writer.toString());
                        }

                        final String outputReply = activitiOperation.execute(inMsgWsdl, this.taskService,
                                this.identityService, this.runTimeService);

                        exchange.setOutMessageContent(new ByteArrayInputStream(outputReply.getBytes("UTF-8")));
                    } else {
                        // TODO: to do
            		}

                } catch (final MessagingException e) {
                    finalException = e;
                } catch (final UnsupportedEncodingException e) {
                    finalException = e;
                } catch (final TransformerException e) {
                    finalException = e;
                }

                // Handle cases where a fault could not be set on the exchange
                if (finalException != null) {
                    logger.finest("Exchange " + exchange.getExchangeId() + " encountered a problem. "
                            + finalException.getMessage());
                    // Technical error, it would be set as a Fault by the CDK
                    exchange.setError(finalException);
                }
            }

            // True to let the CDK close the exchange.
            // False to explicitly return the exchange.
            return true;
        } finally {
            logger.fine("End ActivitiJBIListener.onJBIMessage()");
        }
    }
}