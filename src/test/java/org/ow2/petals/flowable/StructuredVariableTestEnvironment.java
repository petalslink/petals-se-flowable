/**
 * Copyright (c) 2019-2024 Linagora
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
package org.ow2.petals.flowable;

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.apache.mina.util.AvailablePortFinder;
import org.junit.ClassRule;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;

/**
 * Abstract class for unit tests about process using structured variables as JSON
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class StructuredVariableTestEnvironment extends AbstractTestEnvironment {

    protected static final String STRUCTURED_VARIABLE_SU = "start-stop";

    private static final String STRUCTURED_VARIABLE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/structured-variable";

    protected static final QName STRUCTURED_VARIABLE_INTERFACE = new QName(STRUCTURED_VARIABLE_NAMESPACE,
            "structured-variable");

    protected static final QName STRUCTURED_VARIABLE_SERVICE = new QName(STRUCTURED_VARIABLE_NAMESPACE,
            "structured-variable-service");

    protected static final String STRUCTURED_VARIABLE_ENDPOINT = "edpStructuredVariable";

    protected static final QName OPERATION_START = new QName(STRUCTURED_VARIABLE_NAMESPACE, "start");

    protected static final QName OPERATION_APPROVE = new QName(STRUCTURED_VARIABLE_NAMESPACE, "approve");

    protected static final QName OPERATION_UNLOCK = new QName(STRUCTURED_VARIABLE_NAMESPACE, "unlock");

    private static final String ALARM_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/structured-variable/alarm";

    protected static final QName ALARM_INTERFACE = new QName(ALARM_NAMESPACE, "alarm");

    protected static final QName ALARM_SERVICE = new QName(ALARM_NAMESPACE, "alarmService");

    protected static final String ALARM_ENDPOINT = "edpAlarm";

    protected static final QName ALARM_CREATE_OPERATION = new QName(ALARM_NAMESPACE, "create");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "structured-variable";

    protected static final String BPMN_USER = "kermit";

    protected static final String USER_TASK = "handleRequest";

    protected static final String MESSAGE_EVENT_NAME = "myMessageName";

    protected static String getFileIdmEngineConfiguratorCfgFile() {
        return null;
    }

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            // A async job executor is required to process service task
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                    Boolean.TRUE.toString())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_PORT),
                    String.valueOf(
                            AvailablePortFinder.getNextAvailable(FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT)))
            .registerServiceToDeploy(STRUCTURED_VARIABLE_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/structured-variable/structured-variable.wsdl");
                    assertNotNull("WSDl not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            STRUCTURED_VARIABLE_INTERFACE, STRUCTURED_VARIABLE_SERVICE, STRUCTURED_VARIABLE_ENDPOINT,
                            wsdlUrl);

                    final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/structured-variable/startResponse.xsl");
                    assertNotNull("Output XSL 'startResponse.xsl' not found", startResponseXslUrl);
                    serviceConfiguration.addResource(startResponseXslUrl);

                    final URL messageEventReceivedXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/structured-variable/messageEventReceived.xsl");
                    assertNotNull("Output XSL 'messageEventReceived.xsl' not found", messageEventReceivedXslUrl);
                    serviceConfiguration.addResource(messageEventReceivedXslUrl);

                    final URL instanceUnknownXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/structured-variable/instanceUnknown.xsl");
                    assertNotNull("Output XSL 'instanceUnknown.xsl' not found", instanceUnknownXslUrl);
                    serviceConfiguration.addResource(instanceUnknownXslUrl);

                    final URL unexpectedMessageEventXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/structured-variable/unexpectedMessageEvent.xsl");
                    assertNotNull("Output XSL 'unexpectedMessageEvent.xsl' not found", unexpectedMessageEventXslUrl);
                    serviceConfiguration.addResource(unexpectedMessageEventXslUrl);

                    final URL approveRequestResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/structured-variable/approveRequestResponse.xsl");
                    assertNotNull("Output XSL 'approveRequestResponse.xsl' not found", approveRequestResponseXslUrl);
                    serviceConfiguration.addResource(approveRequestResponseXslUrl);

                    final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/structured-variable/structured-variable.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl);
                    serviceConfiguration.addResource(bpmnUrl);

                    final URL alarmWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/structured-variable/alarm.wsdl");
                    assertNotNull("Alarm WSDl not found", alarmWsdlUrl);
                    serviceConfiguration.addResource(alarmWsdlUrl);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "structured-variable.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                    return serviceConfiguration;
                }
            }).registerNativeServiceToDeploy(NATIVE_PROCESSINSTANCES_SVC_CFG, new NativeServiceConfigurationFactory() {

                @Override
                public ServiceConfiguration create(final String nativeEndpointName) {

                    final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("component.wsdl");
                    assertNotNull("Integration servce WSDl not found", nativeServiceWsdlUrl);
                    return new ProvidesServiceConfiguration(ITG_PROCESSINSTANCES_PORT_TYPE,
                            ITG_PROCESSINSTANCES_SERVICE, nativeEndpointName, nativeServiceWsdlUrl);
                }

                @Override
                public QName getNativeService() {
                    return ITG_PROCESSINSTANCES_SERVICE;
                }
            }).registerNativeServiceToDeploy(NATIVE_EXECUTIONS_SVC_CFG, new NativeServiceConfigurationFactory() {

                @Override
                public ServiceConfiguration create(final String nativeEndpointName) {

                    final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("component.wsdl");
                    assertNotNull("Integration servce WSDl not found", nativeServiceWsdlUrl);
                    return new ProvidesServiceConfiguration(ITG_EXECUTIONS_PORT_TYPE, ITG_EXECUTIONS_SERVICE,
                            nativeEndpointName, nativeServiceWsdlUrl);
                }

                @Override
                public QName getNativeService() {
                    return ITG_EXECUTIONS_SERVICE;
                }
            }).registerExternalServiceProvider(ALARM_ENDPOINT, ALARM_SERVICE, ALARM_INTERFACE);

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    protected static final SimpleComponent COMPONENT = new SimpleComponent(COMPONENT_UNDER_TEST);

    protected static Marshaller MARSHALLER;

    protected static Unmarshaller UNMARSHALLER;

    static {
        try {
            final JAXBContext context = JAXBContext.newInstance(
                    org.ow2.petals.se_flowable.unit_test.structured_variable.ObjectFactory.class,
                    org.ow2.petals.se_flowable.unit_test.structured_variable.alarm.ObjectFactory.class,
                    org.ow2.petals.components.flowable.generic._1.ObjectFactory.class);
            UNMARSHALLER = context.createUnmarshaller();
            MARSHALLER = context.createMarshaller();
            MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    @Override
    protected ComponentUnderTest getComponentUnderTest() {
        return COMPONENT_UNDER_TEST;
    }

    /**
     * Convert a JAXB element to bytes
     * 
     * @param jaxbElement
     *            The JAXB element to write as bytes
     */
    protected static byte[] toByteArray(final Object jaxbElement) throws JAXBException, IOException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            MARSHALLER.marshal(jaxbElement, baos);
            return baos.toByteArray();
        } finally {
            baos.close();
        }
    }
}
