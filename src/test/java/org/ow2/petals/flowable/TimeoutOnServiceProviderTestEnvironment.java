/**
 * Copyright (c) 2018-2023 Linagora
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.apache.mina.util.AvailablePortFinder;
import org.junit.ClassRule;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.component.framework.jbidescriptor.generated.MEPType;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;

/**
 * Abstract class for unit tests about process invoking service provider with timeout
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class TimeoutOnServiceProviderTestEnvironment extends AbstractTestEnvironment {

    protected static final String TIMEOUT_SU = "timeout-on-service";

    protected static final String TIMEOUT_SU_HOME = "su/timeout/";

    private static final String TIMEOUT_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/timeout-on-service";

    protected static final QName TIMEOUT_INTERFACE = new QName(TIMEOUT_NAMESPACE, "timeout");

    protected static final QName TIMEOUT_SERVICE = new QName(TIMEOUT_NAMESPACE, "timeout-service");

    protected static final String TIMEOUT_ENDPOINT = "edpTimeoutOnService";

    protected static final QName OPERATION_START_DEFAULT_ASYNC = new QName(TIMEOUT_NAMESPACE, "start-default-async");

    protected static final QName OPERATION_START_SHORT_ASYNC = new QName(TIMEOUT_NAMESPACE, "start-short-async");

    protected static final QName OPERATION_START_LONG_ASYNC = new QName(TIMEOUT_NAMESPACE, "start-long-async");

    protected static final QName OPERATION_START_DEFAULT_SYNC = new QName(TIMEOUT_NAMESPACE, "start-default-sync");

    protected static final QName OPERATION_START_SHORT_SYNC = new QName(TIMEOUT_NAMESPACE, "start-short-sync");

    protected static final QName OPERATION_START_LONG_SYNC = new QName(TIMEOUT_NAMESPACE, "start-long-sync");

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/timeout-on-service/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "edpArchiver";

    protected static final QName ARCHIVER_DEFAULT_SYNC_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiverDefaultSync");

    protected static final QName ARCHIVER_SHORT_SYNC_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiverShortSync");

    protected static final QName ARCHIVER_LONG_SYNC_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiverLongSync");

    protected static final QName ARCHIVER_DEFAULT_ASYNC_OPERATION = new QName(ARCHIVE_NAMESPACE,
            "archiverDefaultAsync");

    protected static final QName ARCHIVER_SHORT_ASYNC_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiverShortAsync");

    protected static final QName ARCHIVER_LONG_ASYNC_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiverLongAsync");

    protected static final long ARCHIVE_SHORT_TIMEOUT = 10000;

    protected static final int ARCHIVE_LONG_TIMEOUT_S = 75;

    protected static final long ARCHIVE_LONG_TIMEOUT = ARCHIVE_LONG_TIMEOUT_S * 1000;

    protected static String getFileIdmEngineConfiguratorCfgFile() {
        return null;
    }

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                    Boolean.TRUE.toString())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP,
                    FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME), "1000")
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP,
                    FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME), "1000")
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_PORT),
                    String.valueOf(
                            AvailablePortFinder.getNextAvailable(FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT)))
            .registerServiceToDeploy(TIMEOUT_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(TIMEOUT_SU_HOME + "timeout.wsdl");
                    assertNotNull("WSDL not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            TIMEOUT_INTERFACE, TIMEOUT_SERVICE, TIMEOUT_ENDPOINT, wsdlUrl);

                    final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(TIMEOUT_SU_HOME + "startResponse.xsl");
                    assertNotNull("Output XSL 'startResponse.xsl' not found", startResponseXslUrl);
                    serviceConfiguration.addResource(startResponseXslUrl);

                    final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(TIMEOUT_SU_HOME + "timeout.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl);
                    serviceConfiguration.addResource(bpmnUrl);

                    final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(TIMEOUT_SU_HOME + "archivageService.wsdl");
                    assertNotNull("archivageService WSDL not found", archivageServiceWsdlUrl);
                    serviceConfiguration.addResource(archivageServiceWsdlUrl);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "timeout.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                    // Consume service 'archiver'
                    final ConsumesServiceConfiguration archiverDefaultSyncConsume = new ConsumesServiceConfiguration(
                            ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                    // MEP 'RobustInOnly' is required to force CXF to wait the service provider ack or fault
                    archiverDefaultSyncConsume.setMEP(MEPType.ROBUST_IN_ONLY);
                    archiverDefaultSyncConsume.setOperation(ARCHIVER_DEFAULT_SYNC_OPERATION);
                    // We use the default timeout
                    serviceConfiguration.addServiceConfigurationDependency(archiverDefaultSyncConsume);

                    final ConsumesServiceConfiguration archiverShortSyncConsume = new ConsumesServiceConfiguration(
                            ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                    // MEP 'RobustInOnly' is required to force CXF to wait the service provider ack or fault
                    archiverShortSyncConsume.setMEP(MEPType.ROBUST_IN_ONLY);
                    archiverShortSyncConsume.setOperation(ARCHIVER_SHORT_SYNC_OPERATION);
                    archiverShortSyncConsume.setTimeout(ARCHIVE_SHORT_TIMEOUT);
                    serviceConfiguration.addServiceConfigurationDependency(archiverShortSyncConsume);

                    final ConsumesServiceConfiguration archiverLongSyncConsume = new ConsumesServiceConfiguration(
                            ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                    // MEP 'RobustInOnly' is required to force CXF to wait the service provider ack or fault
                    archiverLongSyncConsume.setMEP(MEPType.ROBUST_IN_ONLY);
                    archiverLongSyncConsume.setOperation(ARCHIVER_LONG_SYNC_OPERATION);
                    archiverLongSyncConsume.setTimeout(ARCHIVE_LONG_TIMEOUT);
                    serviceConfiguration.addServiceConfigurationDependency(archiverLongSyncConsume);

                    final ConsumesServiceConfiguration archiverDefaultAsyncConsume = new ConsumesServiceConfiguration(
                            ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                    archiverDefaultAsyncConsume.setMEP(MEPType.IN_OUT);
                    archiverDefaultAsyncConsume.setOperation(ARCHIVER_DEFAULT_ASYNC_OPERATION);
                    // We use the default timeout
                    serviceConfiguration.addServiceConfigurationDependency(archiverDefaultAsyncConsume);

                    final ConsumesServiceConfiguration archiverShortAsyncConsume = new ConsumesServiceConfiguration(
                            ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                    archiverShortAsyncConsume.setMEP(MEPType.IN_OUT);
                    archiverShortAsyncConsume.setOperation(ARCHIVER_SHORT_ASYNC_OPERATION);
                    archiverShortAsyncConsume.setTimeout(ARCHIVE_SHORT_TIMEOUT);
                    serviceConfiguration.addServiceConfigurationDependency(archiverShortAsyncConsume);

                    final ConsumesServiceConfiguration archiverLongAsyncConsume = new ConsumesServiceConfiguration(
                            ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                    archiverLongAsyncConsume.setMEP(MEPType.IN_OUT);
                    archiverLongAsyncConsume.setOperation(ARCHIVER_LONG_ASYNC_OPERATION);
                    archiverLongAsyncConsume.setTimeout(ARCHIVE_LONG_TIMEOUT);
                    serviceConfiguration.addServiceConfigurationDependency(archiverLongAsyncConsume);

                    return serviceConfiguration;
                }
            })
            .registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE);

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    protected static final SimpleComponent COMPONENT = new SimpleComponent(COMPONENT_UNDER_TEST);

    protected static Marshaller MARSHALLER;

    protected static Unmarshaller UNMARSHALLER;

    static {
        try {
            final JAXBContext context = JAXBContext
                    .newInstance(org.ow2.petals.se_flowable.unit_test.timeout_on_service.ObjectFactory.class,
                            org.ow2.petals.se_flowable.unit_test.timeout_on_service.archivageservice.ObjectFactory.class);
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
