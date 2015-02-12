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

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;

import javax.jbi.messaging.ExchangeStatus;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.impl.message.WrappedStatusFromConsumerMessage;

/**
 * Unit tests about request processing not dedicated to BPMN services ({@link BpmnServicesInvocationTest}) or
 * integration services ({@link IntegrationServicesInvocationTest}).
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class OtherServicesInvocationTest extends AbstractComponentTest {

    /**
     * <p>
     * Check the processing of the component when receiving the end (status 'DONE') of an IN_OU exchange.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no MONIT trace logged</li>
     * </ul>
     * </p>
     */
    @Test
    public void onDoneStatusAsProvider() {

        // Send an exchange with the status set to 'DONE'. We must use 'processMessageFromServiceBus' because nothing is
        // returned on the end of IN-OUT exchange
        COMPONENT_UNDER_TEST.processMessageFromServiceBus(new WrappedStatusFromConsumerMessage(COMPONENT_UNDER_TEST
                .getServiceConfiguration(VALID_SU), OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream("".getBytes()),
                new FlowAttributes("testFlowInstanceId", "testFlowStepId"), ExchangeStatus.DONE));

        assertEquals(0, IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT).size());
    }

    /**
     * <p>
     * Check the processing of the component when receiving a IN-OUT message exchange with status 'ERROR'.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no MONIT trace logged</li>
     * <li>a warning is logged telling that the message exchange is discarded.</li>
     * </ul>
     * </p>
     */
    @Test
    public void onErrorStatusAsProvider() throws InterruptedException {

        // Send an exchange with the status set to 'ERROR'. We must use 'processMessageFromServiceBus' because nothing
        // is returned on the end of IN-OUT exchange
        COMPONENT_UNDER_TEST.processMessageFromServiceBus(new WrappedStatusFromConsumerMessage(COMPONENT_UNDER_TEST
                .getServiceConfiguration(VALID_SU), OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream("".getBytes()),
                new FlowAttributes("testFlowInstanceId", "testFlowStepId"), ExchangeStatus.ERROR));

        assertEquals(0, IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT).size());
        // A log trace must be logged with level WARNING telling that incoming error messages are not accepted
        assertEquals(1, IN_MEMORY_LOG_HANDLER.getAllRecords(Level.WARNING).size());
    }

}
